{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Fco.Core.Config (
  CfgRequest (..), CfgResponse (..), 
  setupConfig) where

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

import Fco.Core.Util (withData)


type CKey = Text
type CValue = Text
type DSKey = Text
type DataSet = (DSKey, [(CKey, CValue)])

type ConfigStore = HM.HashMap DSKey (HashMap CKey CValue)

data CfgRequest = CfgQuery (SendPort CfgResponse) DSKey 
                | CfgUpdate DSKey CKey CValue
  deriving (Show, Generic, Typeable)
instance Binary CfgRequest

data CfgResponse = CfgResponse DataSet
  deriving (Show, Generic, Typeable)
instance Binary CfgResponse

type CfgReqChan = (SendPort CfgRequest, ReceivePort CfgRequest)
type CfgRespChan = (SendPort CfgResponse, ReceivePort CfgResponse)


setupConfig :: Process (ProcessId, SendPort CfgRequest)
setupConfig = do
    configData <- liftIO loadConfig
    (cfgReqSend, cfgReqRecv) <- newChan :: Process CfgReqChan
    pid <- spawnLocal $ cfgListen (cfgReqRecv, configData)
    return (pid, cfgReqSend)

cfgListen :: (ReceivePort CfgRequest, ConfigStore) -> Process ()
cfgListen =
    withData $ \(cfgReqRecv, cfgData) ->
      receiveChan cfgReqRecv >>= \case
          CfgQuery port key -> do
              sendChan port $ CfgResponse $ getDataFor key cfgData
              return $ Just (cfgReqRecv, cfgData)
          CfgUpdate dskey key value -> 
              return $ Just (cfgReqRecv, updateData dskey key value cfgData)


getDataFor :: DSKey -> ConfigStore -> DataSet
getDataFor dskey cfgData = (dskey, 
                            HM.toList (HM.lookupDefault HM.empty dskey cfgData))

updateData :: DSKey -> CKey -> CValue -> ConfigStore -> ConfigStore
updateData dskey key value cfgData =
    HM.insert dskey (updateDS key value cfgData) cfgData
    where updateDS k v dat = 
            HM.insert k v (HM.lookupDefault HM.empty dskey dat)


loadConfig :: IO ConfigStore
loadConfig = do
    let path = "../data/config-fco.yaml"
        extractString = map (\(String vs) -> vs)
        extractObject f = map (\(Object vo) -> f vo)
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    return $ extractObject extractString conf



-- legacy: load config for Pocket interface

loadPocketConfig :: IO (HM.HashMap DSKey Value)
loadPocketConfig = do
    let path = "../../data/pocket/access.yaml"
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    --return $ maybe HM.empty id conf
    return conf

