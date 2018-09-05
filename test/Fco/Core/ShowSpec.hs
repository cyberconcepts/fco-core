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
      rdfs = (Namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdfs")
      type_ = Node rdf "type"
      class_ = Node rdf "Class"
      label = Node rdfs "label"
      topic = Node fco "topic"
      relevance = Node fco "relevance"

  describe "show functions" $ do
    it "provide printable representations of a node" $ do
      showNode type_ `shouldBe` "rdf:type"
      showNode topic `shouldBe` "fco:topic"
    it "provide printable representations of the object part of a triple" $ do
      showObject (NodeRef topic) `shouldBe` "fco:topic"
      showObject (IntVal 17) `shouldBe` "17"
      showObject (TextVal "Hello World") `shouldBe` "\"Hello World\""
    it "provide printable representations of a triple" $ do
      showTriple (Triple topic type_ (NodeRef class_))
        `shouldBe` "fco:topic rdf:type rdf:Class"
      showTriple (Triple topic label (TextVal "Topic"))
        `shouldBe` "fco:topic rdfs:label \"Topic\""
      showTriple (Triple topic relevance (IntVal 3))
        `shouldBe` "fco:topic fco:relevance 3"
