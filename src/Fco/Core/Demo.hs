{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Demo (run) where

import BasicPrelude

import Control.Concurrent (threadDelay)
import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    getSelfPid, matchChan, newChan, receiveWait, sendChan)
import Control.Monad.Extra (whileM)

import Fco.Core.Config (setupConfigDef)
import Fco.Core.Console (handleConMsg, setupConsole)
import Fco.Core.Messaging (
    CtlChan, CtlMsg (DoQuit, InfoMsg), 
    NotifChan, Notification (RequestQuit),
    runMainProcess)
import Fco.Core.Types (GraphResp)


-- message handlers

handleNotif :: Notification -> Process Bool
handleNotif RequestQuit = return False
handleNotif _ = return True


-- application

run :: IO ()
run = 
  runMainProcess $ do
    (notifSend, notifRecv) <- newChan :: Process NotifChan
    (cfgReqSend, cfgCtlSend) <- setupConfigDef
    (conW, conWSend, conRRecv) <- setupConsole notifSend --cfgReqSend
    whileM $
      receiveWait [
          matchChan notifRecv $ handleNotif,
          matchChan conRRecv $ handleConMsg conWSend
      ]
    sendChan conWSend "stopping application"
    sendChan cfgCtlSend DoQuit
    liftIO $ threadDelay 200000
