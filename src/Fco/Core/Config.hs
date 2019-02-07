{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Fco.Core.Config (
  CfgRequest (..), CfgResponse (..), 
  loadConfig, setupConfig, setupConfigDef,
  loadPocketConfig) where

import BasicPrelude
import qualified Data.Text as T

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import Data.Binary (Binary)
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import System.Directory (doesFileExist, findFile)
import System.Environment (lookupEnv)

import Control.Distributed.Process (Process, SendPort, sendChan)

import Fco.Core.Messaging (
    Message, MsgHandler, ParentId, Service (..), ServiceId,
    defaultService, setupService)

import qualified Fco.Core.Service as Svc
import Fco.Core.Service (Channel,
    defaultCtlHandler, defaultListener, dummyHandler, startService)


type CKey = Text
type CValue = Text
type DSKey = Text
type DataSet = (DSKey, [(CKey, CValue)])

type ConfigStore = HM.HashMap DSKey (HashMap CKey CValue)


-- new implementation, using Fco.Core.ServiceId

type ConfigService = Svc.Service ConfigRequest

type ConfigRespChannel = Channel ConfigResponse

data ConfigRequest = ConfigQuery ConfigRespChannel DSKey
                   | ConfigUpdate DSKey CKey CValue

newtype ConfigResponse = ConfigResponse DataSet


startConfigSvcDefault :: IO ConfigService
startConfigSvcDefault = 
    (lookupEnv "config-fco") >>= \case
      Just path -> startConfigSvc path
      _ -> startConfigSvc "../data/config-fco.yaml"

startConfigSvc :: FilePath -> IO ConfigService
startConfigSvc path = do
    configData <- loadConfig path
    startService defaultListener configHandler configData

configHandler :: Svc.MsgHandler ConfigStore ConfigRequest
configHandler cfgData (Svc.Message (ConfigQuery rchannel key)) = do
    Svc.sendChan rchannel $ Svc.Message (ConfigResponse (getDataFor key cfgData))
    return $ Just cfgData
configHandler cfgData (Svc.Message (ConfigUpdate dskey key value)) = 
    return $ Just $ updateData dskey key value cfgData
configHandler state msg = defaultCtlHandler state msg


-- legacy stuff, using distributed-process

data CfgRequest = CfgQuery (SendPort CfgResponse) DSKey 
                | CfgUpdate DSKey CKey CValue
  deriving (Show, Generic, Typeable)
instance Binary CfgRequest
instance Message CfgRequest

newtype CfgResponse = CfgResponse DataSet
  deriving (Show, Generic, Typeable)
instance Binary CfgResponse
instance Message CfgResponse


setupConfigDef :: ParentId ->
                  Process (SendPort CfgRequest, ServiceId)
setupConfigDef notifSend =
    -- TODO: use getArgs to retrieve path from commandline arguments
    -- TODO: use findFile to check for candidates
    liftIO (lookupEnv "config-fco") >>= \case
      Just path -> setupConfig path notifSend
      _ -> setupConfig "../data/config-fco.yaml" notifSend

setupConfig :: FilePath -> ParentId ->
               Process (SendPort CfgRequest, ServiceId)
setupConfig path parent = do
    configData <- liftIO $ loadConfig path
    let svc = defaultService
                { serviceState = configData,
                  messageHandler = handleRequest }
    setupService svc parent


handleRequest :: MsgHandler ConfigStore CfgRequest
handleRequest self cfgData (CfgQuery client key) = do
    sendChan client $ CfgResponse $ getDataFor key cfgData
    return $ Just cfgData
handleRequest self cfgData (CfgUpdate dskey key value) = 
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
    Right conf <- Yaml.decodeFileEither path :: IO (Either Yaml.ParseException Object)
    return $ extractObject extractString conf


-- legacy: load config for Pocket interface

loadPocketConfig :: IO (HM.HashMap DSKey Value)
loadPocketConfig = do
    let path = "../../data/pocket/access.yaml"
    Right conf <- Yaml.decodeFileEither path :: IO (Either Yaml.ParseException Object)
    --return $ maybe HM.empty id conf
    return conf

