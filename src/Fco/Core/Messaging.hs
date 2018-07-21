{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Messaging where

import BasicPrelude

import Data.Binary (Binary)
import GHC.Generics (Generic)


-- control message

data CtlMsg = QuitMsg
  deriving (Show, Generic, Typeable)
instance Binary CtlMsg


