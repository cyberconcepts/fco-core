{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Core.Service where

import BasicPrelude
import qualified Data.Text as T

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (
    TChan,
    atomically, newTChan, readTChan, writeTChan)
import Control.Monad.Extra (whileM)
import Control.Monad.STM


-- service, channel, message types

type Channel a = TChan (Message a)
type MsgHandler a = Message a -> IO Bool
type Listener a = Channel a -> MsgHandler a -> IO ()

data Message a = Message a | QuitMsg

data Service a = Service (Channel a) ThreadId


-- service functions

startService :: Listener a -> MsgHandler a -> IO (Service a)
startService listener handler = do
    mailbox <- newChan
    pid <- forkIO $ listener mailbox handler
    return $ Service mailbox pid


defaultListener :: Listener a
defaultListener mailbox handler =
  whileM $ do
    msg <- receiveChan mailbox
    handler msg


dummyHandler :: MsgHandler a
dummyHandler msg = return False


send :: Service a -> Message a -> IO ()
send (Service chan _) msg = sendChan chan msg


-- console service

type ConMsg = Message Text

conIn :: Service Text -> Listener Text
conIn client mailbox _ =
  whileM $ do
    line <- getLine
    case line of
      "bye" -> send client QuitMsg >> return False
      _ -> send client (Message line) >> return True

conOutHandler :: MsgHandler Text
conOutHandler QuitMsg = return False
conOutHandler (Message line) = putStrLn line >> return True

demo = do
  conSrv <- startService defaultListener conOutHandler
  startService (conIn conSrv) dummyHandler


-- low-level messaging definitions

newChan :: IO (TChan a)
newChan = atomically newTChan

receiveChan :: TChan msg -> IO msg
receiveChan = atomically . readTChan

sendChan :: TChan msg -> msg -> IO ()
sendChan chan msg = atomically $ writeTChan chan msg
