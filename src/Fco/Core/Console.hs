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
import Control.Monad (forever)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    matchChan, newChan, receiveChan, receiveWait, sendChan, spawnLocal)

import Fco.Core.Messaging (
    Channel, CtlChan, CtlMsg (DoQuit), Notification (AckQuit, RequestQuit))


type ConWChan = Channel Text
type ConRChan = Channel Text


setupConsole :: SendPort Notification -> 
                Process (SendPort Text, ReceivePort Text, SendPort CtlMsg)
setupConsole notifSend = do
    (conWSend, conWRecv) <- newChan :: Process ConWChan
    (conRSend, conRRecv) <- newChan :: Process ConRChan
    (ctlSend, ctlRecv) <- newChan :: Process CtlChan
    conW <- spawnLocal $ conWriter notifSend ctlRecv conWRecv conRSend
    return (conWSend, conRRecv, ctlSend)

conWriter :: SendPort Notification -> ReceivePort CtlMsg ->
             ReceivePort Text -> SendPort Text -> 
             Process ()
conWriter notifSend ctlRecv conWRecv conRSend = do
    conR <- spawnLocal $ conReader notifSend conRSend
    whileM $
      receiveWait [
          matchChan ctlRecv handleControl,
          matchChan conWRecv handleText
      ]

conReader :: SendPort Notification -> SendPort Text -> Process ()
conReader notifSend conRSend =
    whileM $ do
      line <- getLine
      case line of
        "bye" -> sendChan notifSend RequestQuit >> return False
        _ -> sendChan conRSend line >> return True


handleControl :: CtlMsg -> Process Bool
handleControl DoQuit = putStrLn "stopping application" >> return False

handleText :: Text -> Process Bool
handleText txt = putStrLn txt >> return True
