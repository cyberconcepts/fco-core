{-# LANGUAGE OverloadedStrings #-}

module Fco.Core.ServiceSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Fco.Core.Service


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  describe "channel" $ do
    it "accepts and delivers a message" $ do
      c1 <- newChan
      sendChan c1 7 `shouldReturn` ()
      receiveChan c1 `shouldReturn` 7
