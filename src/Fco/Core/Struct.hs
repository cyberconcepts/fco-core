{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Struct where

import BasicPrelude

import Data.Aeson (object, (.=), Object, Value (Object, String))
import qualified Data.HashMap.Strict as HM
import Data.Text (unpack)


lookup :: Text -> HM.HashMap Text a -> a
lookup key hm =
    case HM.lookup key hm of
        Nothing -> error ("Key '" ++ unpack key ++ "' not found.")
        Just v1 -> v1


lookupString :: Text -> HM.HashMap Text Value -> Text
lookupString key hm =
    case HM.lookup key hm of
        Nothing -> error ("Key '" ++ unpack key ++ "' not found.")
        Just v1 -> case v1 of
            String v2 -> v2
            _ -> error ("Value at '" ++ unpack key ++ "' is not a String.")

