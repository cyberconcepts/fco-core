{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Messaging (
    Channel, CtlChan, CtlMsg (..), 
    NotifChan, Notification (..), ParentId, Service (..), ServiceId,
    defaultService, runMainProcess, setupService) where

import BasicPrelude

import Data.Binary (Binary)
import Control.Distributed.Process (
    Process, ReceivePort, SendPort,
    newChan, spawnLocal)
import Control.Distributed.Process.Backend.SimpleLocalnet (
    initializeBackend, newLocalNode)
import Control.Distributed.Process.Node (
    initRemoteTable, runProcess)
import GHC.Generics (Generic)


-- main process

host = "127.0.0.1"
port = "8899"

runMainProcess :: Process () -> IO ()
runMainProcess proc = do
    backend <- initializeBackend host port initRemoteTable
    node <- newLocalNode backend
    runProcess node proc


-- standard message and channel types

class (Binary msg, Typeable msg) => Message msg

data Notification = RequestQuit | AckQuit | InfoNotif Text | ErrorNotif Text
  deriving (Show, Generic, Typeable)
instance Binary Notification

data CtlMsg = DoQuit | InfoMsg Text
  deriving (Show, Generic, Typeable)
instance Binary CtlMsg

type ServiceId = SendPort CtlMsg
type ParentId = SendPort Notification

type Channel a = (SendPort a, ReceivePort a)
type NotifChan = Channel Notification
type CtlChan = Channel CtlMsg


-- service definitions

--data (Message req, Message resp) => Service req resp state = Service {
data Service req resp state = Service {
    parent :: ParentId,
    serviceListener :: ServiceId -> ParentId -> 
                ReceivePort CtlMsg -> ReceivePort req -> state ->
                Process (),
    serviceResponse :: Maybe (SendPort resp),
    serviceState :: state,
    messageHandler :: ServiceId -> state -> req -> Process (Maybe state),
    controlHandler :: ServiceId -> ParentId -> state -> CtlMsg -> 
                Process (Maybe state)
}

defaultService = Service undefined defaultListener Nothing () 
    defaultMessageHandler defaultControlHandler


setupService :: (Message req, Message resp) => Service req resp state -> 
                Process (SendPort req, ServiceId)
setupService serviceDef = do
    (reqSend, reqRecv) <- newChan :: Message req => Process (Channel req)
    (ctlSend, ctlRecv) <- newChan :: Process CtlChan
    spawnLocal $ 
        (serviceListener serviceDef) 
            ctlSend (parent serviceDef) ctlRecv reqRecv $ serviceState serviceDef
    return (reqSend, ctlSend)


defaultListener :: ServiceId -> ParentId -> 
                   ReceivePort CtlMsg -> ReceivePort req -> state ->
                   Process ()
defaultListener = undefined

defaultMessageHandler :: ServiceId -> state -> msg -> 
                         Process (Maybe state)
defaultMessageHandler srvId state msg = return $ Just state

defaultControlHandler :: ServiceId -> ParentId -> state -> CtlMsg -> 
                         Process (Maybe state)
defaultControlHandler srvId parentId state msg = return $ Just state
