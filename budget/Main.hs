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

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptVersion (PlutusScriptV1), ScriptData (ScriptDataNumber), displayError, toPlutusData)
import qualified Data.ByteString.Short as SBS
import Plutus.Contracts.Game (gameSBS, gameSerialised)
import Plutus.Contracts.HelloWorldNumeric (helloWorldSBS, helloWorldSerialised)

import Plutus.V1.Ledger.Api as PlutusAPI
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptnum = if nargs > 0 then read (args !! 0) else 42
  let scriptName = if nargs > 1 then args !! 1 else "result.plutus"
  putStrLn "Guess Game:"
  writePlutusScript scriptnum scriptName gameSBS
  putStrLn "Hello world:"
  writePlutusScript scriptnum scriptName helloWorldSBS


writePlutusScript scriptnum filename scriptSBS = do
  case PlutusAPI.defaultCostModelParams of
    Just m ->
      let pData = toPlutusData (ScriptDataNumber scriptnum)
          (logout, e) = PlutusAPI.evaluateScriptCounting PlutusAPI.Verbose m scriptSBS [pData]
       in do
            print ("Log output" :: String) >> print logout
            case e of
              Left evalErr -> print ("Eval Error" :: String) >> print evalErr
              Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
    Nothing -> error "defaultCostModelParams failed"