{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeApplications   #-}

module Main(main) where

import           Data.Aeson                          (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..))
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Language.PureScript.Bridge          (equal, genericShow, mkSumType)
import           Plutus.Contract                     (ContractError)
import           Plutus.Contracts.Game               as Game
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes (..))

data StarterContracts =
    GameContract
    deriving (Eq, Ord, Show, Generic)

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

instance HasPSTypes StarterContracts where
    psTypes p = [(equal <*> (genericShow <*> mkSumType)) p]

instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [GameContract]
    getSchema =  \case
        GameContract -> Builtin.endpointsToSchemas @Game.GameSchema
    getContract = \case
        GameContract -> Builtin.SomeBuiltin (Game.game @ContractError)

main :: IO ()
main = runWith (Builtin.handleBuiltin @StarterContracts)
