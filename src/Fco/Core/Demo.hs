{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Core.Demo where

import BasicPrelude

import Control.Distributed.Process (
    Process, ProcessId,
    getSelfPid, match, receiveWait, send)
import Control.Distributed.Process.Backend.SimpleLocalnet (
    initializeBackend, newLocalNode)
import Control.Distributed.Process.Node (
    initRemoteTable, runProcess)

import Fco.Core.Config (setupConfig)
import Fco.Core.Console (ConMsg (ConMsg), handleConMsg, setupConsole)
import Fco.Core.Messaging (CtlMsg (QuitMsg))
import Fco.Core.Types (GraphResp)


host = "127.0.0.1"
port = "8899"


-- message handlers

handleQuit :: ProcessId -> CtlMsg -> Process Bool
handleQuit pid QuitMsg = 
    send pid ("bye\n" :: Text) >> return False


-- application

run :: IO ()
run = do
    backend <- initializeBackend host port initRemoteTable
    node <- newLocalNode backend
    runProcess node $ do
      self <- getSelfPid
      configSrv <- setupConfig self
      conW <- setupConsole self
      loop conW
      where 
        loop conW = do
          continue <- receiveWait [
                match $ handleQuit conW, 
                match $ handleConMsg conW]
          case continue of
            True -> loop  conW
            _ -> return ()
