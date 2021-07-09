{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A guessing game
module Plutus.Contracts.Game
  ( lock,
    guess,
    game,
    GameSchema,
    GuessParams (..),
    LockParams (..),

    -- * Scripts
    gameValidator,
    gameSBS,
    gameSerialised,
    hashString,
    clearString,

    -- * Address
    gameAddress,
    validateGuess,

    -- * Traces
    guessTrace,
    lockTrace,
  )
where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptVersion (PlutusScriptV1))
import Codec.Serialise
import Control.Monad (void)
import qualified Control.Monad.Freer.Extras.Log as Extras
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (Address, Datum (Datum), ScriptContext, TxOutTx, Validator, Value)
import qualified Ledger
import qualified Ledger.Ada as Ada
import Ledger.AddressMap (UtxoMap)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract
import Plutus.Contract.Schema ()
import Plutus.Trace.Emulator (EmulatorTrace, observableState)
import qualified Plutus.Trace.Emulator as Trace
import qualified Plutus.V1.Ledger.Scripts as Plutus
import PlutusTx (Data (..))
import qualified PlutusTx
import PlutusTx.Prelude
import Schema (ToArgument, ToSchema)
import Wallet.Emulator (Wallet (..))
import qualified Prelude

newtype HashedString = HashedString ByteString
  deriving newtype (PlutusTx.IsData)
  deriving (Prelude.Show)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString ByteString
  deriving newtype (PlutusTx.IsData)
  deriving (Prelude.Show)

PlutusTx.makeLift ''ClearString

type GameSchema =
  Endpoint "lock" LockParams
    .\/ Endpoint "guess" GuessParams

-- | The validation function (DataValue -> RedeemerValue -> ScriptContext -> Bool)
{-# INLINEABLE validateGuess #-}
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodGuess hs cs

{-# INLINEABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

data Game

instance Scripts.ValidatorTypes Game where
  type RedeemerType Game = ClearString
  type DatumType Game = HashedString

gameInstance :: Scripts.TypedValidator Game
gameInstance =
  Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [||validateGuess||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @HashedString @ClearString

{-
    As a Script
-}

gameScript :: Plutus.Script
gameScript = Plutus.unValidatorScript gameValidator

{-
    As a Short Byte String
-}

gameSBS :: SBS.ShortByteString
gameSBS = SBS.toShort . LBS.toStrict $ serialise gameScript

{-
    As a Serialised Script
-}

-- gameSerialised :: PlutusScript PlutusScriptV1 
gameSerialised = PlutusScriptSerialised gameSBS

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Prelude.String -> HashedString
hashString = HashedString . sha2_256 . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: Prelude.String -> ClearString
clearString = ClearString . C.pack

-- | The address of the game (the hash of its validator script)
gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
  { secretWord :: Prelude.String,
    amount :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
  { guessWord :: Prelude.String
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

game :: (AsContractError e, ToJSON e) => Contract () GameSchema e ()
game = do
  lock `select` guess

lock :: (AsContractError e) => Contract () GameSchema e ()
lock = do
  logInfo @Prelude.String "Waiting for lock endpoint..."
  LockParams secret amt <- endpoint @"lock" @LockParams
  logInfo @Prelude.String $ "Pay " <> Prelude.show amt <> " to the script"
  let tx = Constraints.mustPayToTheScript (hashString secret) amt
  void (submitTxConstraints gameInstance tx)

guess :: (AsContractError e) => Contract () GameSchema e ()
guess = do
  -- Wait for script to have a UTxO of a least 1 lovelace
  logInfo @Prelude.String "Waiting for script to have a UTxO of at least 1 lovelace"
  utxos <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)
  -- Wait for a call on the guess endpoint
  logInfo @Prelude.String "Waiting for guess endpoint..."
  GuessParams theGuess <- endpoint @"guess" @GuessParams

  let redeemer = clearString theGuess
      tx = collectFromScript utxos redeemer

  -- Log a message saying if the secret word was correctly guessed
  let hashedSecretWord = findSecretWordValue utxos
      isCorrectSecretWord = fmap (`isGoodGuess` redeemer) hashedSecretWord == Just True
  if isCorrectSecretWord
    then logWarn "Correct secret word! Submitting the transaction"
    else logWarn "Incorrect secret word, but still submiting the transaction"

  -- This is only for test purposes to have a possible failing transaction.
  -- In a real use-case, we would not submit the transaction if the guess is
  -- wrong.
  logInfo @Prelude.String "Submitting transaction to guess the secret word"
  void (submitTxConstraintsSpending gameInstance utxos tx)

-- | Find the secret word in the Datum of the UTxOs
findSecretWordValue :: UtxoMap -> Maybe HashedString
findSecretWordValue =
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output is possible
secretWordValue :: TxOutTx -> Maybe HashedString
secretWordValue o = do
  dh <- Ledger.txOutDatum $ Ledger.txOutTxOut o
  Datum d <- Map.lookup dh $ Ledger.txData $ Ledger.txOutTxTx o
  PlutusTx.fromData d

lockTrace :: Wallet -> Prelude.String -> EmulatorTrace ()
lockTrace wallet secretWord = do
  hdl <- Trace.activateContractWallet wallet (lock @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock" hdl (LockParams secretWord (Ada.adaValueOf 10))
  void $ Trace.waitNSlots 1

guessTrace :: Wallet -> Prelude.String -> EmulatorTrace ()
guessTrace wallet guessWord = do
  hdl <- Trace.activateContractWallet wallet (guess @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"guess" hdl (GuessParams guessWord)
  void $ Trace.waitNSlots 1
