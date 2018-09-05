{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Types where

import BasicPrelude

import Data.Binary (Binary)
import GHC.Generics (Generic)


-- RDF triples

type IRI = Text
type Prefix = Text

data Namespace = Namespace IRI Prefix 
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Binary Namespace

type NodeName = Text
type Subject = Node
type Predicate = Node

data Node = Node Namespace NodeName 
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Binary Node

data Object = NodeObj NodeName | IntObj Int64 | TextObj Text 
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Binary Object

data Triple = Triple Subject Predicate Object 
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Binary Triple


-- query types

data QuCrit a = IsEqual a | Ignore 
  deriving (Eq, Show, Generic, Typeable)
instance Binary a => Binary (QuCrit a)

data Query = Query (QuCrit Node) (QuCrit Node) (QuCrit Object)
  deriving (Eq, Show, Generic, Typeable)
instance Binary Query


-- message types - actions and responses for (RDF) graphs

data GraphMsg = GraphQuery Query | GraphUpdate Triple | GraphDelete Triple
  deriving (Show, Generic, Typeable)
instance Binary GraphMsg

data GraphResp = GraphResp [Triple]
  deriving (Show, Generic, Typeable)
instance Binary GraphResp

