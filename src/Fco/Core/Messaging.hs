{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Messaging (
    Channel, CtlChan, CtlMsg (..), CtlHandler, 
    Listener, Message, MsgHandler,
    NotifChan, Notification (..), ParentId, Service (..), ServiceId,
    defaultService, runMainProcess, setupService) where

import BasicPrelude

import Data.Binary (Binary)
import Control.Distributed.Process (
    Process, ReceivePort, SendPort,
    matchChan, newChan, receiveWait, sendChan, spawnLocal)
import Control.Distributed.Process.Backend.SimpleLocalnet (
    initializeBackend, newLocalNode)
import Control.Distributed.Process.Node (
    initRemoteTable, runProcess)
import GHC.Generics (Generic)

import Fco.Core.Util (whileDataM)


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

data Notification = RequestQuit ServiceId 
                  | AckQuit ServiceId
                  | InfoNotif ServiceId Text 
                  | ErrorNotif ServiceId Text
  deriving (Show, Generic, Typeable)
instance Binary Notification
instance Message Notification

data CtlMsg = DoQuit | InfoMsg Text
  deriving (Show, Generic, Typeable)
instance Binary CtlMsg
instance Message CtlMsg

type ServiceId = SendPort CtlMsg
type ParentId = SendPort Notification

type Channel a = (SendPort a, ReceivePort a)
type NotifChan = Channel Notification
type CtlChan = Channel CtlMsg

type Listener state req = ServiceId -> ParentId -> 
                  ReceivePort CtlMsg -> ReceivePort req -> 
                  CtlHandler state -> MsgHandler state req -> state ->
                  Process ()
type MsgHandler state req = ServiceId -> state -> req -> 
                  Process (Maybe state)
type CtlHandler state = ServiceId -> ParentId -> state -> CtlMsg -> 
                  Process (Maybe state)


-- service definitions

data Message req => Service req state = Service {
    serviceListener :: Listener state req,
    serviceState :: state,
    messageHandler :: MsgHandler state req,
    controlHandler :: CtlHandler state
}

defaultService :: Message req => Service req state
defaultService = Service defaultListener undefined
        defaultMessageHandler defaultControlHandler


setupService :: Message req => Service req state -> 
                ParentId -> Process (SendPort req, ServiceId)
setupService svc parent = do
    (reqSend, reqRecv) <- newChan :: Message req => Process (Channel req)
    (ctlSend, ctlRecv) <- newChan :: Process CtlChan
    spawnLocal $ 
        (serviceListener svc) 
            ctlSend parent ctlRecv reqRecv 
            (controlHandler svc) (messageHandler svc)
            (serviceState svc)
    return (reqSend, ctlSend)


defaultListener :: Listener state req
defaultListener self parent ctlRecv reqRecv handleControl handleRequest = 
    whileDataM $ \state ->
      receiveWait [
          matchChan ctlRecv $ handleControl self parent state,
          matchChan reqRecv $ handleRequest self state
      ]

defaultMessageHandler :: MsgHandler state req
defaultMessageHandler self state msg = return $ Just state

defaultControlHandler :: CtlHandler state
defaultControlHandler self parent state DoQuit = do
    sendChan parent $ AckQuit self
    return Nothing
defaultControlHandler self parent state msg = return $ Just state
