{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Contracts.HelloWorldNumeric
  ( helloWorldSerialised,
    helloWorldSBS,
  )
where

import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V1.Ledger.Scripts as Plutus
import PlutusTx (Data (..))
import qualified PlutusTx
import PlutusTx.Prelude as P hiding (Semigroup (..), unless)
import Prelude hiding (($))

{-
  The "hello world" message as a data item
-}

hello :: Data
hello = I 123

{-
   The Hello World validator script
-}

{-# INLINEABLE helloWorld #-}
helloWorld :: Data -> Data -> Data -> ()
helloWorld datum _ _ = if datum P.== hello then () else (P.error ())

{-
    As a Validator
-}

helloWorldValidator :: Plutus.Validator
helloWorldValidator = Plutus.mkValidatorScript $$(PlutusTx.compile [||helloWorld||])

{-
    As a Script
-}

helloWorldScript :: Plutus.Script
helloWorldScript = Plutus.unValidatorScript helloWorldValidator

{-
    As a Short Byte String
-}

helloWorldSBS :: SBS.ShortByteString
helloWorldSBS = SBS.toShort . LBS.toStrict $ serialise helloWorldScript

{-
    As a Serialised Script
-}

-- helloWorldSerialised :: PlutusScript PlutusScriptV1
helloWorldSerialised = PlutusScriptSerialised helloWorldSBS