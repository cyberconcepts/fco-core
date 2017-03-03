{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Core.Types where

import BasicPrelude

-- types

data Namespace = Namespace IRI Prefix

data Node = Node Namespace Name

data Triple = Triple Subject Predicate Object

data Object = TextObj Text | NodeObj Node

type Name = Text
type IRI = Text
type Prefix = Text

type Subject = Node
type Predicate = Node
