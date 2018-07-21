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

import Data.Binary (Binary)
import Control.Monad (forever)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    newChan, receiveChan, sendChan, spawnLocal)

import Fco.Core.Messaging (CtlMsg (QuitMsg))


type ConWChan = (SendPort Text, ReceivePort Text)
type ConRChan = (SendPort Text, ReceivePort Text)


setupConsole :: SendPort CtlMsg
      -> Process (ProcessId, SendPort Text, ReceivePort Text)
setupConsole ctlSend = do
    (conWSend, conWRecv) <- newChan :: Process ConWChan
    (conRSend, conRRecv) <- newChan :: Process ConRChan
    conW <- spawnLocal $ conWriter ctlSend conWRecv conRSend
    return (conW, conWSend, conRRecv)

conWriter :: SendPort CtlMsg -> ReceivePort Text -> SendPort Text -> Process ()
conWriter ctlSend conWRecv conRSend = do
    conR <- spawnLocal $ conReader ctlSend conRSend
    forever $ receiveChan conWRecv >>= putStrLn

conReader :: SendPort CtlMsg -> SendPort Text -> Process ()
conReader ctlSend conRSend =
    forever $ do
      line <- getLine
      case line of
        "bye" -> sendChan ctlSend QuitMsg
        _ -> sendChan conRSend line


handleConMsg :: SendPort Text -> Text -> Process Bool
handleConMsg port txt = 
    sendChan port txt >> return True
