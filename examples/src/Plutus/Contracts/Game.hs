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
{-# LANGUAGE OverloadedStrings          #-}
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

import           Control.Monad                  (void)
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.Map                       as Map
import           GHC.Generics                   (Generic)
import           Ledger                         (Address, Datum (Datum), ScriptContext, TxOutTx, Validator, Value)
import qualified Ledger
import qualified Ledger.Ada                     as Ada
import           Ledger.AddressMap              (UtxoMap)
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import           Plutus.Contract
import           Plutus.Contract.Schema         ()
import           Plutus.Trace.Emulator          (EmulatorTrace, observableState)
import qualified Plutus.Trace.Emulator          as Trace
import qualified PlutusTx
import           PlutusTx.Prelude
import           Schema                         (ToArgument, ToSchema)
import           Wallet.Emulator                (Wallet (..))

import qualified Control.Monad.Freer.Extras.Log as Extras
import qualified Data.ByteString.Char8          as C
import           Data.Maybe                     (catMaybes)
import           Data.Void                      (Void)
import qualified Prelude                        as Haskell

newtype HashedString = HashedString ByteString
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving Haskell.Show

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString ByteString
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving Haskell.Show

PlutusTx.makeLift ''ClearString

type GameSchema =
        Endpoint "lock" LockParams
        .\/ Endpoint "guess" GuessParams

-- | The validation function (DataValue -> RedeemerValue -> ScriptContext -> Bool)
{-# INLINABLE validateGuess #-}
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = traceIfFalse "Bad guess" (isGoodGuess hs cs)

{-# INLINABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @HashedString @ClearString

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: Haskell.String -> ClearString
clearString = ClearString . C.pack

-- | The address of the game (the hash of its validator script)
gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: Haskell.String
    , amount     :: Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: Haskell.String
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

game :: (AsContractError e, ToJSON e) => Contract () GameSchema e ()
game = do
  logInfo @Haskell.String "Waiting for guess or lock endpoint..."
  selectList [lock, guess]

lock :: (AsContractError e) => Promise () GameSchema e ()
lock = endpoint @"lock" @LockParams $ \(LockParams secret amt) -> do
    logInfo @Haskell.String $ "Pay " <> Haskell.show amt <> " to the script"
    let tx         = Constraints.mustPayToTheScript (hashString secret) amt
    void (submitTxConstraints gameInstance tx)

guess :: (AsContractError e) => Promise () GameSchema e ()
guess = endpoint @"guess" @GuessParams $ \(GuessParams theGuess) -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)

    let redeemer = clearString theGuess
        tx       = collectFromScript utxos redeemer

    -- Log a message saying if the secret word was correctly guessed
    let hashedSecretWord = findSecretWordValue utxos
        isCorrectSecretWord = fmap (`isGoodGuess` redeemer) hashedSecretWord == Just True
    logWarn @Haskell.String
      $ if isCorrectSecretWord
         then "Correct secret word! Submitting the transaction"
         else "Incorrect secret word, but still submiting the transaction"

    -- This is only for test purposes to have a possible failing transaction.
    -- In a real use-case, we would not submit the transaction if the guess is
    -- wrong.
    logInfo @Haskell.String "Submitting transaction to guess the secret word"
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
  PlutusTx.fromBuiltinData d

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
