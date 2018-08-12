{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Demo (run) where

import BasicPrelude

import Control.Concurrent (threadDelay)
import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    getSelfPid, matchChan, newChan, 
    receiveChanTimeout, receiveWait, sendChan)
import Control.Monad.Extra (whileM)

import Fco.Core.Config (setupConfigDef)
import Fco.Core.Console (setupConsole)
import Fco.Core.Messaging (
    CtlChan, CtlMsg (DoQuit, InfoMsg), 
    NotifChan, Notification (RequestQuit),
    runMainProcess)
import Fco.Core.Types (GraphResp)


-- message handlers

handleNotif :: Notification -> Process Bool
handleNotif RequestQuit = return False
handleNotif _ = return True

handleConMsg :: SendPort Text -> Text -> Process Bool
handleConMsg port txt = 
    sendChan port txt >> return True


-- application

run :: IO ()
run = 
  runMainProcess $ do
    (notifSend, notifRecv) <- newChan :: Process NotifChan
    (cfgReqSend, cfgCtlSend) <- setupConfigDef notifSend
    (conWSend, conRRecv, conCtlSend) <- setupConsole notifSend --cfgReqSend
    whileM $
      receiveWait [
          matchChan notifRecv $ handleNotif,
          matchChan conRRecv $ handleConMsg conWSend
      ]
    sendChan cfgCtlSend DoQuit
    sendChan conCtlSend DoQuit
    receiveChanTimeout 1000000 notifRecv >>= print
    receiveChanTimeout 1000000 notifRecv >>= print
    --liftIO $ threadDelay 200000
