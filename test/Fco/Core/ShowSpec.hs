{-# LANGUAGE OverloadedStrings #-}

module Fco.Core.ShowSpec (main, spec) where

import Test.Hspec

import Fco.Core.Show
import Fco.Core.Types


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  let fco = (Namespace "http://functionalconcepts.org/fco-common#" "fco")
      rdf = (Namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf")
      topic = Node fco "topic"

  describe "dummy" $ do
    it "is just for demonstration" $ do
      "foo bar" `shouldBe` "foo bar"

  describe "showNode" $ do
    it "provides printable representation of a node" $
      showNode topic `shouldBe` "fco:topic"

