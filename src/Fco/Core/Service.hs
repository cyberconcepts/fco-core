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

type Channel = TChan
type MsgHandler msg = msg -> IO Bool
type Listener msg = Channel msg -> MsgHandler msg -> IO ()

data Service msg = Service (Channel msg) ThreadId


-- service functions

startService :: Listener msg -> MsgHandler msg -> IO (Service msg)
startService listener handler = do
    mailbox <- newChan
    pid <- forkIO $ listener mailbox handler
    return $ Service mailbox pid


defaultListener :: Listener msg
defaultListener mailbox handler =
  whileM $ do
    msg <- receiveChan mailbox
    handler msg


dummyHandler :: MsgHandler msg
dummyHandler msg = return False


send :: Service msg -> msg -> IO ()
send (Service chan _) msg = sendChan chan msg


-- console service

data ConMsg = ConMsg Text | QuitMsg

conIn :: Service ConMsg -> Listener Text
conIn client mailbox _ =
  whileM $ do
    line <- getLine
    case line of
      "bye" -> send client QuitMsg >> return False
      _ -> send client (ConMsg line) >> return True

conOut :: Listener ConMsg
conOut mailbox _ =
  whileM $ do
    msg <- receiveChan mailbox
    case msg of
      QuitMsg -> return False
      ConMsg line -> putStrLn line >> return True

conOutHandler QuitMsg = return False
conOutHandler (ConMsg line) = putStrLn line >> return True

demo = do
  conSrv <- startService conOut dummyHandler
  startService (conIn conSrv) dummyHandler


-- low-level messaging definitions

newChan :: IO (TChan a)
newChan = atomically newTChan

receiveChan :: TChan a -> IO a
receiveChan = atomically . readTChan

sendChan :: TChan a -> a -> IO ()
sendChan chan msg = atomically $ writeTChan chan msg


