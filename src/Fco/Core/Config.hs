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

import Control.Distributed.Process (Process, SendPort, sendChan)

import Fco.Core.Messaging (
    Listener, Message, MsgHandler, ParentId, Service (..), ServiceId,
    defaultService, setupService)


type CKey = Text
type CValue = Text
type DSKey = Text
type DataSet = (DSKey, [(CKey, CValue)])

type ConfigStore = HM.HashMap DSKey (HashMap CKey CValue)

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
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    return $ extractObject extractString conf


-- legacy: load config for Pocket interface

loadPocketConfig :: IO (HM.HashMap DSKey Value)
loadPocketConfig = do
    let path = "../../data/pocket/access.yaml"
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    --return $ maybe HM.empty id conf
    return conf

