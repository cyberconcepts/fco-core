{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Fco.Core.Config (
  startConfigSvc, startConfigSvcDefault,
  loadConfig, loadPocketConfig) where

import BasicPrelude
import qualified Data.Text as T

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist, findFile)
import System.Environment (lookupEnv)

import Fco.Core.Service (
    Channel, Message (..), MsgHandler, Service,
    defaultCtlHandler, defaultListener, dummyHandler, sendChan, startService)


type CKey = Text
type CValue = Text
type DSKey = Text
type DataSet = (DSKey, [(CKey, CValue)])

type ConfigStore = HM.HashMap DSKey (HashMap CKey CValue)


-- config service

type ConfigService = Service ConfigRequest

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

configHandler :: MsgHandler ConfigStore ConfigRequest
configHandler cfgData (Message (ConfigQuery rchannel key)) = do
    sendChan rchannel $ Message (ConfigResponse (getDataFor key cfgData))
    return $ Just cfgData
configHandler cfgData (Message (ConfigUpdate dskey key value)) = 
    return $ Just $ updateData dskey key value cfgData
configHandler state msg = defaultCtlHandler state msg


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

