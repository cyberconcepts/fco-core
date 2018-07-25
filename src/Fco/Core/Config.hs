{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Config (
  CfgRequest (..), CfgResponse (..), setupConfig) where

import BasicPrelude
import qualified Data.Text as T

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import Data.Binary (Binary)
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml
import Control.Monad (forever)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    expect, getSelfPid, match, newChan, receiveChan, receiveWait, 
    send, sendChan, spawnLocal)


data CfgRequest = CfgQuery (SendPort CfgResponse) Text | CfgUpdate Text Text
  deriving (Show, Generic, Typeable)
instance Binary CfgRequest

data CfgResponse = CfgResponse [(Text, Text)]
  deriving (Show, Generic, Typeable)
instance Binary CfgResponse

type CfgReqChan = (SendPort CfgRequest, ReceivePort CfgRequest)
type CfgRespChan = (SendPort CfgResponse, ReceivePort CfgResponse)

type ConfigStore = HM.HashMap Text Value


setupConfig :: Process (ProcessId, SendPort CfgRequest)
setupConfig = do
    configData <- liftIO loadConfig
    (cfgReqSend, cfgReqRecv) <- newChan :: Process CfgReqChan
    pid <- spawnLocal $ cfgListen cfgReqRecv configData
    return (pid, cfgReqSend)

cfgListen :: ReceivePort CfgRequest -> ConfigStore -> Process ()
cfgListen cfgReqRecv cfgData =
    loop cfgReqRecv cfgData
    where
      loop cfgReqRecv cfgData = do
        msg <- receiveChan cfgReqRecv
        case msg of
          CfgQuery port key -> do
              sendChan port $ CfgResponse $ getDataFor key cfgData
              loop cfgReqRecv cfgData
          CfgUpdate key value -> 
              loop cfgReqRecv $ updateData key value cfgData

getDataFor key cfgData = []

updateData :: Text -> Text -> ConfigStore -> ConfigStore
updateData key value cfgData = cfgData

loadConfig :: IO ConfigStore
loadConfig = do
    let path = "../data/config-fco.yaml"
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    --return $ maybe HM.empty id conf
    return conf


-- legacy: load config for Pocket interface

loadPocketConfig :: IO ConfigStore
loadPocketConfig = do
    let path = "../../data/pocket/access.yaml"
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    --return $ maybe HM.empty id conf
    return conf

