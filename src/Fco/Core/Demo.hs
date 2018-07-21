{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Demo (run) where

import BasicPrelude

import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    getSelfPid, matchChan, newChan, receiveWait, sendChan)

import Fco.Core.Config (setupConfig)
import Fco.Core.Console (handleConMsg, setupConsole)
import Fco.Core.Messaging (CtlChan, CtlMsg (QuitMsg), runMain)
import Fco.Core.Types (GraphResp)


-- message handlers

handleQuit :: SendPort Text -> CtlMsg -> Process Bool
handleQuit port QuitMsg = 
    sendChan port "stopping application\n" >> return False


-- application

run :: IO ()
run = 
  runMain $ do
    self <- getSelfPid
    (ctlSend, ctlRecv) <- newChan :: Process CtlChan
    configSrv <- setupConfig self
    -- (cfgSrv, cfgReqSend) <- setupConfig self
    (conW, conWSend, conRRecv) <- setupConsole ctlSend --cfgReqSend
    loop (ctlRecv, conRRecv, conWSend)
    where 
      loop params@(ctlRecv, conRRecv, conWSend) = do
        continue <- receiveWait [
              matchChan ctlRecv $ handleQuit conWSend,
              matchChan conRRecv $ handleConMsg conWSend]
        case continue of
          True -> loop params
          _ -> return ()
