{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Parse where

import BasicPrelude
import Data.Char (isDigit)
import qualified Data.Text as T

import Fco.Core.Types


type NsLookup = Prefix -> Namespace


parseNode :: NsLookup -> Text -> Node
parseNode nsLookup txt =
    let (prefix, rname) = T.breakOn ":" txt
        ns = nsLookup prefix
    in Node ns (T.tail rname)

parseObject :: NsLookup -> Text -> Object
parseObject nsLookup txt = 
    case parseIntVal txt of 
      Just n -> IntVal n
      _ -> case parseTextVal txt of 
            Just txt -> TextVal txt
            _ -> NodeRef $ parseNode nsLookup txt

parseTriple :: NsLookup -> Text -> Triple
parseTriple nsLookup txt =
    let (st, pt, ot) = splitTripleString txt
        s = parseNode nsLookup st
        p = parseNode nsLookup pt
        o = parseObject nsLookup ot
    in Triple s p o

parseQuery :: NsLookup -> Text -> Query
parseQuery nsLookup txt =
    let (st, pt, ot) = splitTripleString txt
        s = parseQuCrit nsLookup st parseNode
        p = parseQuCrit nsLookup pt parseNode
        o = parseQuCrit nsLookup ot parseObject
    in Query s p o

parseQuCrit :: NsLookup -> Text -> (NsLookup -> Text -> a) -> QuCrit a
parseQuCrit nsLookup txt pfct = 
    case txt of
          "?" -> Ignore
          _ -> IsEqual $ pfct nsLookup txt

-- helper functions

parseTextVal :: Text -> Maybe Text
parseTextVal txt = 
    case T.uncons txt of
        Just ('"', t1) -> Just $ stripTrailing t1
        _ -> Nothing
    where 
        stripTrailing "" = ""
        stripTrailing t =
            case T.last t of
                '"' -> T.init t
                _ -> t

parseIntVal :: Text -> Maybe Int
parseIntVal txt =
    case T.all isDigit txt of
        True -> Just $ read txt
        False -> Nothing

splitTripleString :: Text -> (Text, Text, Text)
splitTripleString txt = 
    let stripSpace = snd . T.span (== ' ')
        txt1 = stripSpace txt
        (st, r1) = T.breakOn " " txt1
        r2 = stripSpace r1
        (pt, r3) = T.breakOn " " r2
        ot = stripSpace r3
    in (st, pt, ot)

