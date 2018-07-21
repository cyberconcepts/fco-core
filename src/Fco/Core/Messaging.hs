{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Messaging (CtlChan, CtlMsg (..), runMain) where

import BasicPrelude

import Data.Binary (Binary)
import Control.Distributed.Process (Process, ReceivePort, SendPort)
import Control.Distributed.Process.Backend.SimpleLocalnet (
    initializeBackend, newLocalNode)
import Control.Distributed.Process.Node (
    initRemoteTable, runProcess)
import GHC.Generics (Generic)


host = "127.0.0.1"
port = "8899"


runMain :: Process () -> IO ()
runMain proc = do
    backend <- initializeBackend host port initRemoteTable
    node <- newLocalNode backend
    runProcess node proc


-- standard message types

data CtlMsg = QuitMsg
  deriving (Show, Generic, Typeable)
instance Binary CtlMsg


type CtlChan = (SendPort CtlMsg, ReceivePort CtlMsg)
