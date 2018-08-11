{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Messaging (
    Channel, CtlChan, CtlMsg (..), NotifChan, Notification (..),
    runMainProcess) where

import BasicPrelude

import Data.Binary (Binary)
import Control.Distributed.Process (Process, ReceivePort, SendPort)
import Control.Distributed.Process.Backend.SimpleLocalnet (
    initializeBackend, newLocalNode)
import Control.Distributed.Process.Node (
    initRemoteTable, runProcess)
import GHC.Generics (Generic)


type Channel a = (SendPort a, ReceivePort a)


host = "127.0.0.1"
port = "8899"


runMainProcess :: Process () -> IO ()
runMainProcess proc = do
    backend <- initializeBackend host port initRemoteTable
    node <- newLocalNode backend
    runProcess node proc


-- standard message and channel types

data Notification = RequestQuit | AckQuit | InfoNotif Text | ErrorNotif Text
  deriving (Show, Generic, Typeable)
instance Binary Notification

type NotifChan = Channel Notification


data CtlMsg = DoQuit | InfoMsg Text
  deriving (Show, Generic, Typeable)
instance Binary CtlMsg

type CtlChan = Channel CtlMsg

