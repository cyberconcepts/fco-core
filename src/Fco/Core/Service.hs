{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fco.Core.Service where

import BasicPrelude
import qualified Data.Text as T

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (
    TChan,
    atomically, newTChan, readTChan, writeTChan)
import Control.Monad.Extra (whileM)
import Control.Monad.STM

import Fco.Core.Util (whileDataM)


-- service, channel, message types

type Channel a = TChan (Message a)
type MsgHandler st a = st -> Message a -> IO (Maybe st)
type Listener st a = Channel a -> MsgHandler st a -> st -> IO ()

data Message a = Message a | QuitMsg

data Service a = Service (Channel a) ThreadId


-- service functions

startService :: Listener st a -> MsgHandler st a -> st -> IO (Service a)
startService listener handler state = do
    mailbox <- newChan
    pid <- forkIO $ listener mailbox handler state
    return $ Service mailbox pid


defaultListener :: Listener st a
defaultListener mailbox handler =
  whileDataM $ \state ->
    receiveChan mailbox >>= (handler state)


dummyHandler :: MsgHandler st a
dummyHandler state (Message _) = return $ Just state
dummyHandler state msg = defaultCtlHandler state msg

defaultCtlHandler :: MsgHandler st a
defaultCtlHandler _ QuitMsg = return Nothing
defaultCtlHandler state _ = return $ Just state


send :: Service a -> Message a -> IO ()
send (Service chan _) msg = sendChan chan msg

receive :: Service a -> IO (Message a)
receive (Service chan _) = receiveChan chan


-- console service

type ConMsg = Message Text

conIn :: Service Text -> Listener () Text
conIn parent mailbox _ _ =
    whileM $ getLine >>= \case
        "bye" -> send parent QuitMsg >> return False
        line -> send parent (Message line) >> return True

conOutHandler :: MsgHandler () Text
conOutHandler _ (Message line) = putStrLn line >> (return $ Just ())
conOutHandler _ msg = defaultCtlHandler () msg

demo = do
    conSrv <- startService defaultListener conOutHandler ()
    startService (conIn conSrv) dummyHandler ()


-- low-level messaging definitions

newChan :: IO (TChan a)
newChan = atomically newTChan

receiveChan :: TChan msg -> IO msg
receiveChan = atomically . readTChan

sendChan :: TChan msg -> msg -> IO ()
sendChan chan msg = atomically $ writeTChan chan msg
