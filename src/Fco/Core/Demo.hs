{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Demo (run) where

import BasicPrelude

import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    getSelfPid, matchChan, newChan, receiveWait, sendChan)
import Control.Monad.Extra (whileM)

import Fco.Core.Config (setupConfigDef)
import Fco.Core.Console (handleConMsg, setupConsole)
import Fco.Core.Messaging (CtlChan, CtlMsg (QuitMsg), runMainProcess)
import Fco.Core.Types (GraphResp)


-- message handlers

handleQuit :: SendPort Text -> CtlMsg -> Process Bool
handleQuit port QuitMsg = 
    sendChan port "stopping application\n" >> return False


-- application

run :: IO ()
run = 
  runMainProcess $ do
    (ctlSend, ctlRecv) <- newChan :: Process CtlChan
    (cfgSrv, cfgReqSend) <- setupConfigDef
    (conW, conWSend, conRRecv) <- setupConsole ctlSend --cfgReqSend
    whileM $
      receiveWait [
          matchChan ctlRecv $ handleQuit conWSend,
          matchChan conRRecv $ handleConMsg conWSend]
