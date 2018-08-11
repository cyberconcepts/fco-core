{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Console (
  ConWChan, ConRChan,
  handleConMsg, setupConsole) where

import BasicPrelude
import qualified Data.Text as T

import Control.Monad.Extra (whileM)
import Data.Binary (Binary)
import Control.Monad (forever)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    newChan, receiveChan, sendChan, spawnLocal)

import Fco.Core.Messaging (
    Channel, CtlMsg (DoQuit), Notification (AckQuit, RequestQuit))


type ConWChan = Channel Text
type ConRChan = Channel Text


setupConsole :: SendPort Notification
      -> Process (ProcessId, SendPort Text, ReceivePort Text)
setupConsole notifSend = do
    (conWSend, conWRecv) <- newChan :: Process ConWChan
    (conRSend, conRRecv) <- newChan :: Process ConRChan
    conW <- spawnLocal $ conWriter notifSend conWRecv conRSend
    return (conW, conWSend, conRRecv)

conWriter :: SendPort Notification -> 
             ReceivePort Text -> SendPort Text -> 
             Process ()
conWriter notifSend conWRecv conRSend = do
    conR <- spawnLocal $ conReader notifSend conRSend
    forever $ receiveChan conWRecv >>= putStrLn

conReader :: SendPort Notification -> SendPort Text -> Process ()
conReader notifSend conRSend =
    whileM $ do
      line <- getLine
      case line of
        "bye" -> sendChan notifSend RequestQuit >> return False
        _ -> sendChan conRSend line >> return True


handleConMsg :: SendPort Text -> Text -> Process Bool
handleConMsg port txt = 
    sendChan port txt >> return True
