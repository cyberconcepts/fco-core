{-# LANGUAGE OverloadedStrings #-}

module Fco.Core.StructSpec (main, spec) where

import Test.Hspec

import Fco.Core.Struct

import Control.Exception (evaluate)
import Data.Aeson (object, (.=), Object, Value (Object, String))
import qualified Data.HashMap.Strict as HM


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


testData1 = HM.fromList [("key1", String "value"),
                         ("key2", Object HM.empty)]


spec :: Spec
spec = do

  describe "dummy" $ do
    it "is just for demonstration" $ do
      "foo bar" `shouldBe` "foo bar"
  
  describe "lookupString" $ do
    it "gets a (JSON) String value from a hashmap" $ do
      lookupString "key1" testData1 `shouldBe` "value"
    it "throws an error in case of a type mismatch" $ do
      evaluate (lookupString "key2" testData1)
          `shouldThrow` anyException
    it "returns an empty string if key is not found" $ do
      lookupString "foo" testData1 `shouldBe` ""
