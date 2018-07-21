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
    expect, getSelfPid, match, receiveWait, send, spawnLocal)


-- config data service

type ConfigStore = HM.HashMap Text Value

data CfgMsg = CfgQuery ProcessId Text | CfgUpdate Text Text
  deriving (Show, Generic, Typeable)
instance Binary CfgMsg

setupConfig :: ProcessId -> Process ProcessId
setupConfig parent = do
    config <- liftIO loadConfig
    spawnLocal (cfgListen parent config) >>= return

cfgListen :: ProcessId -> ConfigStore -> Process ()
cfgListen parent config = do
    forever $ do 
      msg <- (expect :: Process CfgMsg)
      return ()

handleCfgMsg :: ProcessId -> CfgMsg -> Process Bool
handleCfgMsg pid msg = 
    send pid msg >> return True


-- legacy: load config for Pocket interface

loadConfig :: IO ConfigStore
loadConfig = do
    let path = "../../data/pocket/access.yaml"
    Just conf <- Yaml.decodeFile path :: IO (Maybe Object)
    --return $ maybe HM.empty id conf
    return conf

