{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Core.TestData where

import BasicPrelude

import Fco.Core.Types


fco = (Namespace "http://functionalconcepts.org/fco-common#" "fco")
rdf = (Namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf")
rdfs = (Namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdfs")

type_ = Node rdf "type"
class_ = Node rdf "Class"
label = Node rdfs "label"

topic = Node fco "topic"
priority = Node fco "priority"

namespaces = [fco, rdf, rdfs]

findNsByPrefix :: Prefix -> Namespace
findNsByPrefix prefix = 
    case find checkPrefix namespaces of
        Just ns -> ns
        Nothing -> error $ "Namespace " ++ (show prefix) ++ " not found!"
    where checkPrefix (Namespace iri pf) = pf == prefix
