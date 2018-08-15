{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Console (
  ConWChan, ConRChan,
  setupConsole) where

import BasicPrelude
import qualified Data.Text as T

import Control.Monad.Extra (whileM)
import Data.Binary (Binary)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    matchChan, newChan, receiveChan, receiveWait, sendChan, spawnLocal)

import Fco.Core.Messaging (
    Channel, CtlChan, CtlMsg (DoQuit), 
    Listener, Message, MsgHandler, Notification (AckQuit, RequestQuit),
    ParentId, Service (..), ServiceId,
    defaultService, setupService)


type ConWChan = Channel Text
type ConRChan = Channel Text

instance Message Text


setupConsole :: ParentId -> 
                Process (SendPort Text, ReceivePort Text, ServiceId)
setupConsole parent = do
    let svc = defaultService { 
                serviceState = (), 
                messageHandler = handleText }
    (conWSend, ctlSend) <- setupService svc parent
    (conRSend, conRRecv) <- newChan :: Process ConRChan
    conR <- spawnLocal $ conReader parent conRSend
    return (conWSend, conRRecv, ctlSend)

handleText :: MsgHandler () Text
handleText self _ txt = do 
    putStrLn txt
    return $ Just ()


conReader :: ParentId -> SendPort Text -> Process ()
conReader parent client =
    whileM $ do
      line <- getLine
      case line of
        "bye" -> sendChan parent RequestQuit >> return False
        _ -> sendChan client line >> return True


