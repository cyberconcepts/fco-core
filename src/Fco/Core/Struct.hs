{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Struct where

import BasicPrelude

import Data.Aeson (object, (.=), Object, Value (Object, String))
import qualified Data.HashMap.Strict as HM


lookupString :: Text -> HM.HashMap Text Value -> Text
lookupString key hm =
    let String value = HM.lookupDefault (String "") key hm
    in value
