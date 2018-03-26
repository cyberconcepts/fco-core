{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Config where

import BasicPrelude

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml


loadConfig :: IO (HM.HashMap Text Value)
loadConfig = do
    let path = "../../data/pocket/access.yaml"
    conf <- Yaml.decodeFile path :: IO (Maybe Object)
    let value = case conf of
          Just v -> v
          Nothing -> HM.empty
    return value


