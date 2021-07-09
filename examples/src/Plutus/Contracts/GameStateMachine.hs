{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

-- | A guessing game that
--
--   * Uses a state machine to keep track of the current secret word
--   * Uses a token to keep track of who is allowed to make a guess
module Plutus.Contracts.GameStateMachine
  ( contract,
    typedValidator,
    GameToken,
    mkValidator,
    mintingPolicy,
    LockArgs (..),
    GuessArgs (..),
    GameStateMachineSchema,
    GameError,
    token,
  )
where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as C
import GHC.Generics (Generic)
import Ledger hiding (to)
import Ledger.Constraints (TxConstraints)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as V
import Plutus.Contract
import Plutus.Contract.Schema ()
import Plutus.Contract.StateMachine (State (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import Plutus.Trace.Emulator (EmulatorTrace, observableState)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..), check)
import Schema (ToArgument, ToSchema)
import qualified Prelude as Haskell

newtype HashedString = HashedString ByteString
  deriving newtype (PlutusTx.IsData)
  deriving (Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString ByteString
  deriving newtype (PlutusTx.IsData)
  deriving (Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''ClearString

-- | Arguments for the @"lock"@ endpoint
data LockArgs = LockArgs
  { -- | The secret
    lockArgsSecret :: Haskell.String,
    -- | Value that is locked by the contract initially
    lockArgsValue :: Value
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)

-- | Arguments for the @"guess"@ endpoint
data GuessArgs = GuessArgs
  { -- | The guess
    guessArgsOldSecret :: Haskell.String,
    -- | The new secret
    guessArgsNewSecret :: Haskell.String,
    -- | How much to extract from the contract
    guessArgsValueTakenOut :: Value
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)

-- | The schema of the contract. It consists of the two endpoints @"lock"@
--   and @"guess"@ with their respective argument types.
type GameStateMachineSchema =
  Endpoint "lock" LockArgs
    .\/ Endpoint "guess" GuessArgs

data GameError
  = GameContractError ContractError
  | GameSMError SM.SMContractError
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Top-level contract, exposing both endpoints.
contract :: Contract () GameStateMachineSchema GameError ()
contract = (lock `select` guess) >> contract

-- | The token that represents the right to make a guess
newtype GameToken = GameToken {unGameToken :: Value}
  deriving newtype (Eq, Haskell.Show)

token :: MonetaryPolicyHash -> TokenName -> Value
token mps tn = V.singleton (V.mpsSymbol mps) tn 1

-- | State of the guessing game
data GameState
  = -- | Initial state. In this state only the 'MintTokens' action is allowed.
    Initialised MonetaryPolicyHash TokenName HashedString
  | -- | Funds have been locked. In this state only the 'Guess' action is
    --   allowed.
    Locked MonetaryPolicyHash TokenName HashedString
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

-- instance Eq GameState where
--   {-# INLINEABLE (==) #-}
--   (Initialised sym tn s) == (Initialised sym' tn' s') = sym == sym' && s == s' && tn == tn'
--   (Locked sym tn s) == (Locked sym' tn' s') = sym == sym' && s == s' && tn == tn'
--   _ == _ = traceIfFalse "states not equal" False

-- | Check whether a 'ClearString' is the preimage of a
--   'HashedString'
checkGuess :: HashedString -> ClearString -> Bool
checkGuess (HashedString actual) (ClearString gss) = actual == sha2_256 gss

-- | Inputs (actions)
data GameInput
  = -- | Mint the "guess" token
    MintToken
  | -- | Make a guess, extract the funds, and lock the remaining funds using a
    --   new secret word.
    Guess ClearString HashedString Value
  deriving stock (Haskell.Show, Generic)

{-# INLINEABLE transition #-}
transition :: State GameState -> GameInput -> Maybe (TxConstraints Void Void, State GameState)
transition State {stateData = oldData, stateValue = oldValue} input = case (oldData, input) of
  (Initialised mph tn s, MintToken) ->
    let constraints = Constraints.mustForgeCurrency mph tn 1
     in Just
          ( constraints,
            State
              { stateData = Locked mph tn s,
                stateValue = oldValue
              }
          )
  (Locked mph tn currentSecret, Guess theGuess nextSecret takenOut)
    | checkGuess currentSecret theGuess ->
      let constraints = Constraints.mustSpendAtLeast (token mph tn) <> Constraints.mustForgeCurrency mph tn 0
       in Just
            ( constraints,
              State
                { stateData = Locked mph tn nextSecret,
                  stateValue = oldValue - takenOut
                }
            )
  _ -> Nothing

type GameStateMachine = SM.StateMachine GameState GameInput

{-# INLINEABLE machine #-}
machine :: GameStateMachine
machine = SM.mkStateMachine Nothing transition isFinal
  where
    isFinal _ = False

{-# INLINEABLE mkValidator #-}
mkValidator :: Scripts.ValidatorType GameStateMachine
mkValidator = SM.mkValidator machine

typedValidator :: Scripts.TypedValidator GameStateMachine
typedValidator =
  Scripts.mkTypedValidator @GameStateMachine
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator

mintingPolicy :: Scripts.MonetaryPolicy
mintingPolicy = Scripts.forwardingMonetaryPolicy typedValidator

client :: SM.StateMachineClient GameState GameInput
client = SM.mkStateMachineClient $ SM.StateMachineInstance machine typedValidator

-- | The @"guess"@ endpoint.
guess :: Contract () GameStateMachineSchema GameError ()
guess = do
  GuessArgs {guessArgsOldSecret, guessArgsNewSecret, guessArgsValueTakenOut} <- mapError GameContractError $ endpoint @"guess"

  let guessedSecret = ClearString (C.pack guessArgsOldSecret)
      newSecret = HashedString (sha2_256 (C.pack guessArgsNewSecret))

  void $
    mapError GameSMError $
      SM.runStep
        client
        (Guess guessedSecret newSecret guessArgsValueTakenOut)

lock :: Contract () GameStateMachineSchema GameError ()
lock = do
  LockArgs {lockArgsSecret, lockArgsValue} <- mapError GameContractError $ endpoint @"lock"
  let secret = HashedString (sha2_256 (C.pack lockArgsSecret))
      sym = Scripts.forwardingMonetaryPolicyHash typedValidator
  _ <- mapError GameSMError $ SM.runInitialise client (Initialised sym "guess" secret) lockArgsValue
  void $ mapError GameSMError $ SM.runStep client MintToken

PlutusTx.unstableMakeIsData ''GameState
PlutusTx.makeLift ''GameState
PlutusTx.unstableMakeIsData ''GameInput
PlutusTx.makeLift ''GameInput
PlutusTx.makeLift ''GameToken
PlutusTx.unstableMakeIsData ''GameToken
