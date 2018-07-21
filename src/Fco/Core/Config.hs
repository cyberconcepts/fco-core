{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Config where

import BasicPrelude
import qualified Data.Text as T

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import Data.Binary (Binary)
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml
import Control.Monad (forever)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Process, ProcessId,
    expect, getSelfPid, match, newChan, receiveChan, receiveWait, 
    send, sendChan, spawnLocal)


type ConfigStore = HM.HashMap Text Value

data CfgRequest = CfgQuery ProcessId Text | CfgUpdate Text Text
  deriving (Show, Generic, Typeable)
instance Binary CfgRequest

data CfgResponse = CfgResponse Text
  deriving (Show, Generic, Typeable)
instance Binary CfgResponse


setupConfig :: ProcessId -> Process ProcessId
setupConfig parent = do
    configData <- liftIO loadConfig
    spawnLocal $ cfgListen parent configData

cfgListen :: ProcessId -> ConfigStore -> Process ()
cfgListen parent cfgData =
    forever $ do 
      msg <- (expect :: Process CfgRequest)
      return ()


-- legacy: load config for Pocket interface

loadConfig :: IO ConfigStore
loadConfig = do
    let path = "../../data/pocket/access.yaml"
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    --return $ maybe HM.empty id conf
    return conf

