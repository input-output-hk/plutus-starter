{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- | A guessing game
module Plutus.Contracts.Game
    ( lock
    , guess
    , game
    , GameSchema
    , GuessParams(..)
    , LockParams(..)
    -- * Scripts
    , gameValidator
    , hashString
    , clearString
    -- * Address
    , gameAddress
    , validateGuess
    -- * Traces
    , guessTrace
    , lockTrace
    ) where

import           Control.Monad          (void)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           Ledger                 (Address, Validator, ValidatorCtx, Value)
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Plutus.Contract
import           Plutus.Contract.Schema ()
import           Plutus.Trace.Emulator  (EmulatorTrace)
import qualified Plutus.Trace.Emulator  as Trace
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Data.Map as Map
import           Schema                 (ToArgument, ToSchema)
import           Wallet.Emulator        (Wallet (..))

import qualified Ledger
import qualified Ledger.Ada             as Ada

import qualified Data.ByteString.Char8  as C
import qualified Prelude
import Ledger.AddressMap (UtxoMap)
import Data.Void (Void)

newtype HashedString = HashedString ByteString deriving newtype PlutusTx.IsData

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString ByteString deriving newtype PlutusTx.IsData

PlutusTx.makeLift ''ClearString

type GameSchema =
    BlockchainActions
        .\/ Endpoint "lock" LockParams
        .\/ Endpoint "guess" GuessParams

-- | The validation function (DataValue -> RedeemerValue -> ValidatorCtx -> Bool)
{-# INLINABLE validateGuess #-}
validateGuess :: HashedString -> ClearString -> ValidatorCtx -> Bool
validateGuess (HashedString actual) (ClearString guess') _ = actual == sha2_256 guess'

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

data Game
instance Scripts.ScriptType Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

gameInstance :: Scripts.ScriptInstance Game
gameInstance = Scripts.validator @Game
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @HashedString @ClearString

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: String -> HashedString
hashString = HashedString . sha2_256 . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: String -> ClearString
clearString = ClearString . C.pack

-- | The address of the game (the hash of its validator script)
gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: String
    , amount     :: Value
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: String
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

lock :: AsContractError e => Contract () GameSchema e ()
lock = do
    LockParams secret amt <- endpoint @"lock" @LockParams
    let tx         = Constraints.mustPayToTheScript (hashString secret) amt
    void (submitTxConstraints gameInstance tx)

guess :: AsContractError e => Contract () GameSchema e ()
guess = do
    unspentOutputs <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)
    GuessParams theGuess <- endpoint @"guess" @GuessParams
    let redeemer = clearString theGuess
        tx       = collectFromScript unspentOutputs redeemer
    void (submitTxConstraintsSpending gameInstance unspentOutputs tx)

game :: (AsContractError e, ToJSON e) => Contract () GameSchema e ()
game = do
  lock `select` guess

lockTrace :: Wallet -> String -> EmulatorTrace ()
lockTrace wallet secretWord = do
    hdl <- Trace.activateContractWallet wallet (lock @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"lock" hdl (LockParams secretWord (Ada.lovelaceValueOf 10))
    void $ Trace.waitNSlots 1

guessTrace :: Wallet -> String -> EmulatorTrace ()
guessTrace wallet guessWord = do
    hdl <- Trace.activateContractWallet wallet (guess @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"guess" hdl (GuessParams guessWord)
    void $ Trace.waitNSlots 1
