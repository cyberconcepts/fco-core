{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Show where

import BasicPrelude

import Fco.Core.Types


showNode :: Node -> Text
showNode (Node (Namespace iri prefix) name) = 
    prefix ++ ":" ++ name

