{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Core.Types where

import BasicPrelude

-- types

data Namespace = Namespace IRI Prefix deriving (Eq, Ord, Show)

data Node = Node Namespace Name deriving (Eq, Ord, Show)

data Triple = Triple Subject Predicate Object deriving (Eq, Ord)

data Object = NodeObj Node | TextObj Text deriving (Eq, Ord, Show)

type Name = Text
type IRI = Text
type Prefix = Text

type Subject = Node
type Predicate = Node
