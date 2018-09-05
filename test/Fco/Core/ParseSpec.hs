{-# LANGUAGE OverloadedStrings #-}

module Fco.Core.ParseSpec (main, spec) where

import Test.Hspec

import Control.Exception (evaluate)

import Fco.Core.Parse
import Fco.Core.Types
import Fco.Core.TestData (
        namespaces, findNsByPrefix,
        type_, class_, label, topic, relevance)



-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

nsLookup = findNsByPrefix


spec :: Spec
spec = do

  describe "parse functions" $ do
    it "parse the text representation of a node" $ do
      parseNode nsLookup "rdf:type" `shouldBe` type_
      parseNode nsLookup "fco:topic" `shouldBe` topic
    it "parse the text representation of the object part of a triple" $ do
      parseObject nsLookup "rdf:Class" `shouldBe` NodeRef class_
      parseObject nsLookup "17" `shouldBe` IntVal 17
      parseObject nsLookup "\"Topic\"" `shouldBe` TextVal "Topic"
      parseObject nsLookup "\"Topic" `shouldBe` TextVal "Topic"
      --evaluate (parseObject nsLookup "Topic") 
      --    `shouldThrow` errorCall "Namespace \"Topic\" not found!"
    it "parse the text representation of a triple" $ do
      parseTriple nsLookup  "fco:topic rdf:type rdf:Class"
          `shouldBe` Triple topic type_ (NodeRef class_)
      parseTriple nsLookup  "fco:topic rdfs:label \"Topic\""
          `shouldBe` Triple topic label (TextVal "Topic")
      parseTriple nsLookup  "fco:topic fco:relevance 3"
          `shouldBe` Triple topic relevance (IntVal 3)
