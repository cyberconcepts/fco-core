{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Types where

import BasicPrelude


-- RDF triples

type IRI = Text
type Prefix = Text

data Namespace = Namespace IRI Prefix 
  deriving (Eq, Ord, Show)

type NodeName = Text
type Subject = Node
type Predicate = Node

data Node = Node Namespace NodeName 
  deriving (Eq, Ord, Show)

data Object = NodeRef Node | IntVal Int | TextVal Text 
  deriving (Eq, Ord, Show)

data Triple = Triple Subject Predicate Object 
  deriving (Eq, Ord, Show)


-- query types

data QuCrit a = IsEqual a | Ignore 
  deriving (Eq, Show)

data Query = Query (QuCrit Node) (QuCrit Node) (QuCrit Object)
  deriving (Eq, Show)
