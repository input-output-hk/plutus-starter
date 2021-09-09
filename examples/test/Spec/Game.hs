{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Spec.Game
    ( tests
    ) where

import           Control.Monad         (void)
import           Ledger                (ValidationError(ScriptFailure))
import qualified Ledger.Ada            as Ada
import           Plutus.Contract       (Contract, ContractError(WalletError))
import           Wallet.API            (WalletAPIError(ValidationError))
import           Plutus.Contract.Test
import           Plutus.Contracts.Game
import           Plutus.Trace.Emulator (ContractInstanceTag)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import           Test.Tasty
import qualified Test.Tasty.HUnit      as HUnit
import Prelude

t1, t2 :: ContractInstanceTag
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2

theContract :: Contract () GameSchema ContractError ()
theContract = game

-- W1 locks funds, W2 (and other wallets) should have access to guess endpoint
-- No funds locked, so W2 (and other wallets) should not have access to guess endpoint
tests :: TestTree
tests = testGroup "game"
    [ checkPredicate "Expose 'lock' endpoint, and 'guess' endpoint"
        (endpointAvailable @"lock" theContract t1
          .&&. (endpointAvailable @"guess" theContract t1))
        $ void $ Trace.activateContractWallet w1 (lock @ContractError)

    , checkPredicate "'lock' endpoint submits a transaction"
        (anyTx theContract t1)
        $ do
            hdl <- Trace.activateContractWallet w1 theContract
            Trace.callEndpoint @"lock" hdl (LockParams "secret" (Ada.adaValueOf 10))

    , checkPredicate "'guess' endpoint is available after locking funds"
        (endpointAvailable @"guess" theContract t2)
        $ do
          void $ Trace.activateContractWallet w2 theContract
          lockTrace w1 "secret"

    , checkPredicate "guess right (unlock funds)"
        (walletFundsChange w2 (Ada.adaValueOf 10)
          .&&. walletFundsChange w1 (Ada.adaValueOf (-10)))
        $ do
          lockTrace w1 "secret"
          guessTrace w2 "secret"

    , checkPredicate "guess wrong"
        (walletFundsChange w2 (Ada.lovelaceValueOf 0)
          .&&. walletFundsChange w1 (Ada.adaValueOf (-10))
          .&&. assertContractError guess t2 appropriateError "error should be: WalletError (ValidationError (ScriptFailure _)) ")
        $ do
          lockTrace w1 "secret"
          guessTrace w2 "SECRET"

    , goldenPir "examples/test/Spec/game.pir" $$(PlutusTx.compile [|| validateGuess ||])

    , HUnit.testCase "script size is reasonable" (reasonable gameValidator 20000)
    ]

appropriateError :: ContractError -> Bool
appropriateError e = case e of
    WalletError (ValidationError (ScriptFailure _)) -> True
    _ -> False
