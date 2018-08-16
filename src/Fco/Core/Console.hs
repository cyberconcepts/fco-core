{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Console (setupConsole) where

import BasicPrelude
import qualified Data.Text as T

import Control.Monad.Extra (whileM)
import Data.Binary (Binary)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Process, ReceivePort, SendPort,
    newChan, sendChan, spawnLocal)

import Fco.Core.Messaging (
    Channel, CtlChan, Message, MsgHandler, Notification (RequestQuit),
    ParentId, Service (..), ServiceId,
    defaultService, setupService)


type ConRChan = Channel Text

instance Message Text


setupConsole :: ParentId -> 
                Process (SendPort Text, ReceivePort Text, ServiceId, ServiceId)
setupConsole parent = do
    let svc = defaultService { 
                serviceState = (), 
                messageHandler = handleText }
    (conWSend, ctlWSend) <- setupService svc parent
    (conRRecv, ctlRSend) <- setupConReader parent
    return (conWSend, conRRecv, ctlWSend, ctlRSend)

handleText :: MsgHandler () Text
handleText self _ txt = do 
    putStrLn txt
    return $ Just ()


setupConReader :: ParentId -> Process (ReceivePort Text, ServiceId)
setupConReader parent = do
    (conRSend, conRRecv) <- newChan :: Process ConRChan
    (ctlSend, ctlRecv) <- newChan :: Process CtlChan
    spawnLocal $ conReader ctlSend parent conRSend
    return (conRRecv, ctlSend)

conReader :: ServiceId -> ParentId -> SendPort Text -> Process ()
conReader self parent client =
    whileM $ do
      line <- getLine
      case line of
        "bye" -> sendChan parent (RequestQuit self) >> return False
        _ -> sendChan client line >> return True


