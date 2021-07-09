{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where
import           System.Environment
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptVersion (PlutusScriptV1),toPlutusData,displayError, writeFileTextEnvelope,ScriptData(ScriptDataNumber),toPlutusData)
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import Plutus.Contract (ContractError)
import Plutus.Contracts.Game (gameSBS, gameSerialised)
import Plutus.Contracts.Game as Game
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (PABError (..))
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Plutus.V1.Ledger.Api as PlutusAPI
import Wallet.Emulator.Types (Wallet (..))
import qualified Data.ByteString.Short as SBS
main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptnum = if nargs > 0 then read (args !! 0) else 42
  let scriptName = if nargs > 1 then args !! 1 else "result.plutus"

  case PlutusAPI.defaultCostModelParams of
        Just m ->
          let pData = toPlutusData (ScriptDataNumber scriptnum)
              (logout, e) = PlutusAPI.evaluateScriptCounting PlutusAPI.Verbose m gameSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  -- result <- writeFileTextEnvelope scriptName Nothing gameSerialised
  -- case result of
  --   Left err -> print $ displayError err
  --   Right () -> return ()
  -- writePlutusScript scriptnum scriptName gameSerialised gameSBS
  -- Simulator.runSimulationWith handlers $ do
  --   Simulator.logString @(Builtin StarterContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
  --   shutdown <- PAB.Server.startServerDebug
  --   -- Example of spinning up a game instance on startup
  --   -- You can add simulator actions here:
  --   -- Simulator.observableState
  --   -- etc.
  --   -- That way, the simulation gets to a predefined state and you don't have to
  --   -- use the HTTP API for setup.

  --   -- void $ Simulator.activateContract (Wallet 1) GameContract
  --   -- Pressing enter results in the balances being printed
  --   void $ liftIO getLine
  --   Simulator.logString @(Builtin StarterContracts) "Balances at the end of the simulation"

  --   b <- Simulator.currentBalances
  --   Simulator.logBalances @(Builtin StarterContracts) b

  --   shutdown

data StarterContracts
  = GameContract
  -- | StateMachineGame
  deriving (Eq, Ord, Show, Generic)
  -- deriving anyclass (ToJSON, FromJSON)

-- NOTE: Because 'StarterContracts' only has one constructor, corresponding to
-- the demo 'Game' contract, we kindly ask aeson to still encode it as if it had
-- many; this way we get to see the label of the contract in the API output!
-- If you simple have more contracts, you can just use the anyclass deriving
-- statement on 'StarterContracts' instead:
--
--    `... deriving anyclass (ToJSON, FromJSON)`
instance ToJSON StarterContracts where
  toJSON = genericToJSON defaultOptions {
             tagSingleConstructors = True }
instance FromJSON StarterContracts where
  parseJSON = genericParseJSON defaultOptions {
             tagSingleConstructors = True }

instance Pretty StarterContracts where
  pretty = viaShow

-- writePlutusScript :: Integer -> FilePath -> PlutusScript (PlutusScriptVersion PlutusScriptV1) -> SBS.ShortByteString -> IO ()
-- writePlutusScript scriptnum scriptName gameSerialised gameSBS =
--   do
--   case PlutusAPI.defaultCostModelParams of
--         Just m ->
--           let pData = toPlutusData (ScriptDataNumber scriptnum)
--               (logout, e) = PlutusAPI.evaluateScriptCounting PlutusAPI.Verbose m gameSBS [pData]
--           in do print ("Log output" :: String) >> print logout
--                 case e of
--                   Left evalErr -> print ("Eval Error" :: String) >> print evalErr
--                   Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
--         Nothing -> error "defaultCostModelParams failed"
--   result <- writeFileTextEnvelope scriptName Nothing gameSerialised
--   case result of
--     Left err -> print $ displayError err
--     Right () -> return ()

handleStarterContract ::
  ( Member (Error PABError) effs,
    Member (LogMsg (PABMultiAgentMsg (Builtin StarterContracts))) effs
  ) =>
  ContractEffect (Builtin StarterContracts)
    ~> Eff effs
handleStarterContract = Builtin.handleBuiltin getSchema getContract
  where
    getSchema = \case
      GameContract -> Builtin.endpointsToSchemas @Game.GameSchema
    getContract = \case
      GameContract -> SomeBuiltin (Game.game @ContractError)

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin StarterContracts) [GameContract] $
    interpret handleStarterContract
