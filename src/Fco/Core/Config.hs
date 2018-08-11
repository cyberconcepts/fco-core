{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Fco.Core.Config (
  CfgRequest (..), CfgResponse (..), 
  setupConfig, setupConfigDef) where

import BasicPrelude
import qualified Data.Text as T

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import Data.Binary (Binary)
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import System.Directory (doesFileExist, findFile)
import System.Environment (lookupEnv)

import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    expect, getSelfPid, match, matchChan, newChan, receiveChan, receiveWait, 
    send, sendChan, spawnLocal)

import Fco.Core.Messaging (Channel, CtlChan, CtlMsg (DoQuit))
import Fco.Core.Util (whileDataM)


type CKey = Text
type CValue = Text
type DSKey = Text
type DataSet = (DSKey, [(CKey, CValue)])

type ConfigStore = HM.HashMap DSKey (HashMap CKey CValue)

data CfgRequest = CfgQuery (SendPort CfgResponse) DSKey 
                | CfgUpdate DSKey CKey CValue
  deriving (Show, Generic, Typeable)
instance Binary CfgRequest

newtype CfgResponse = CfgResponse DataSet
  deriving (Show, Generic, Typeable)
instance Binary CfgResponse

type CfgReqChan = Channel CfgRequest
type CfgRespChan = Channel CfgResponse


setupConfigDef :: Process (SendPort CfgRequest, SendPort CtlMsg)
setupConfigDef =
    -- TODO: use getArgs to retrieve path from commandline arguments
    -- TODO: use findFile to check for candidates
    liftIO (lookupEnv "config-fco") >>= \case
      Just path -> setupConfig path
      _ -> setupConfig "../data/config-fco.yaml"

setupConfig :: FilePath -> Process (SendPort CfgRequest, SendPort CtlMsg)
setupConfig path = do
    configData <- liftIO $ loadConfig path
    (cfgReqSend, cfgReqRecv) <- newChan :: Process CfgReqChan
    (cfgCtlSend, cfgCtlRecv) <- newChan :: Process CtlChan
    pid <- spawnLocal $ cfgListen cfgReqRecv cfgCtlRecv configData
    return (cfgReqSend, cfgCtlSend)

cfgListen :: ReceivePort CfgRequest -> 
             ReceivePort CtlMsg -> 
             ConfigStore -> 
             Process ()
cfgListen reqRecv ctlRecv = 
    whileDataM $ \cfgData ->
      receiveWait [
          matchChan ctlRecv handleControl,
          matchChan reqRecv $ handleRequest cfgData
      ]

handleControl :: CtlMsg -> Process (Maybe ConfigStore)
handleControl DoQuit = return Nothing

handleRequest :: ConfigStore -> CfgRequest -> Process (Maybe ConfigStore)
handleRequest cfgData (CfgQuery port key) = do
    sendChan port $ CfgResponse $ getDataFor key cfgData
    return $ Just cfgData
handleRequest cfgData (CfgUpdate dskey key value) = 
    return $ Just $ updateData dskey key value cfgData


-- storage handling

getDataFor :: DSKey -> ConfigStore -> DataSet
getDataFor dskey cfgData = 
    (dskey, HM.toList (HM.lookupDefault HM.empty dskey cfgData))

updateData :: DSKey -> CKey -> CValue -> ConfigStore -> ConfigStore
updateData dskey key value cfgData =
    HM.insert dskey (updateDS key value cfgData) cfgData
    where updateDS k v dat = 
            HM.insert k v (HM.lookupDefault HM.empty dskey dat)


loadConfig :: FilePath -> IO ConfigStore
loadConfig path = do
    let extractString = fmap (\(String vs) -> vs)
        extractObject f = fmap (\(Object vo) -> f vo)
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    return $ extractObject extractString conf


-- legacy: load config for Pocket interface

loadPocketConfig :: IO (HM.HashMap DSKey Value)
loadPocketConfig = do
    let path = "../../data/pocket/access.yaml"
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    --return $ maybe HM.empty id conf
    return conf

