{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Show where

import BasicPrelude
import qualified Data.Text as T

import Fco.Core.Types


showNode :: Node -> Text
showNode (Node (Namespace iri prefix) name) = 
    prefix ++ ":" ++ name

showObject :: Object -> Text
showObject (NodeRef node) = showNode node
showObject (TextVal txt) = "\"" ++ txt ++ "\""
showObject (IntVal i) = T.pack $ show i

showTriple :: Triple -> Text
showTriple (Triple subject predicate object) =
    unwords [(showNode subject), 
             (showNode predicate), 
             (showObject object)]
