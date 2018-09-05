{-# LANGUAGE OverloadedStrings #-}

module Fco.Core.ParseSpec (main, spec) where

import Test.Hspec

import Fco.Core.Parse


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  describe "dummy" $ do
    it "is just for demonstration" $ do
      "foo bar" `shouldBe` "foo bar"
  
