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
    , correctGuessTrace
    ) where

import           Cardano.Node.Emulator.Params (testnet)
import           Control.Lens          (_2, (^?))
import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Plutus.V2.Ledger.Api  (ScriptContext, Validator, Value, Datum(Datum))
import           Ledger                (CardanoAddress, DecoratedTxOut)
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import qualified Ledger
import           Playground.Contract
import           Plutus.Contract
import           Plutus.Contract.Trace as X
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell
import           Plutus.Trace.Emulator (EmulatorTrace)
import qualified Plutus.Trace.Emulator as Trace
import qualified Plutus.Script.Utils.V2.Address as Address
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as V1
import qualified Ledger.Typed.Scripts as Script

newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

type GameSchema =
        Endpoint "lock" LockParams
        .\/ Endpoint "guess" GuessParams

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = V1.mkUntypedValidator @ScriptContext @HashedString @ClearString

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: Haskell.String -> ClearString
clearString = ClearString . toBuiltin . C.pack

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodGuess hs cs

isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

-- | The address of the game (the hash of its validator script)
gameAddress :: CardanoAddress
gameAddress = Address.mkValidatorCardanoAddress testnet $ Script.validatorScript gameInstance

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: !Haskell.String
    , amount     :: !Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: Haskell.String
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | The "lock" contract endpoint. See note [Contract endpoints]
lock :: AsContractError e => Promise () GameSchema e ()
lock = endpoint @"lock" @LockParams $ \(LockParams secret amt) -> do
    logInfo @Haskell.String $ "Pay " <> Haskell.show amt <> " to the script"
    let tx = Constraints.mustPayToTheScriptWithDatumInTx (hashString secret) amt
    void (submitTxConstraints gameInstance tx)

-- | The "guess" contract endpoint. See note [Contract endpoints]
guess :: AsContractError e => Promise () GameSchema e ()
guess = endpoint @"guess" @GuessParams $ \(GuessParams theGuess) -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)

    let redeemer = clearString theGuess
        tx       = Constraints.collectFromTheScript utxos redeemer

    -- Log a message saying if the secret word was correctly guessed
    let hashedSecretWord = findSecretWordValue utxos
        isCorrectSecretWord = fmap (`isGoodGuess` redeemer) hashedSecretWord == Just True
    if isCorrectSecretWord
        then logWarn @Haskell.String "Correct secret word! Submitting the transaction"
        else logWarn @Haskell.String "Incorrect secret word, but still submiting the transaction"

    -- This is only for test purposes to have a possible failing transaction.
    -- In a real use-case, we would not submit the transaction if the guess is
    -- wrong.
    logInfo @Haskell.String "Submitting transaction to guess the secret word"
    void (submitTxConstraintsSpending gameInstance utxos tx)

-- | Find the secret word in the Datum of the UTxOs
-- | Find the secret word in the Datum of the UTxOs
findSecretWordValue :: Map TxOutRef DecoratedTxOut -> Maybe HashedString
findSecretWordValue =
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output is possible
secretWordValue :: DecoratedTxOut -> Maybe HashedString
secretWordValue o = do
  Datum d <- o ^? Ledger.decoratedTxOutDatum . _2 . Ledger.datumInDatumFromQuery
  PlutusTx.fromBuiltinData d

game :: AsContractError e => Contract () GameSchema e ()
game = do
    logInfo @Haskell.String "Waiting for guess or lock endpoint..."
    selectList [lock, guess] >> game

lockTrace :: Wallet -> Haskell.String -> EmulatorTrace ()
lockTrace wallet secretWord = do
    hdl <- Trace.activateContractWallet wallet (lock @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"lock" hdl (LockParams secretWord (Ada.adaValueOf 10))
    void $ Trace.waitNSlots 1

guessTrace :: Wallet -> Haskell.String -> EmulatorTrace ()
guessTrace wallet guessWord = do
    hdl <- Trace.activateContractWallet wallet (guess @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"guess" hdl (GuessParams guessWord)
    void $ Trace.waitNSlots 1

correctGuessTrace :: EmulatorTrace ()
correctGuessTrace = do
  let w1 = X.knownWallet 1
      w2 = X.knownWallet 2
      secret = "secret"

  h1 <- Trace.activateContractWallet w1 (lock @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock" h1 (LockParams secret (Ada.adaValueOf 10))

  h2 <- Trace.activateContractWallet w2 (guess @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"guess" h2 (GuessParams secret)
  void $ Trace.waitNSlots 1
