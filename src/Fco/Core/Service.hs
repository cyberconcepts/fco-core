{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module Fco.Core.Service where

import BasicPrelude
import qualified Data.Text as T

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (
    STM, TChan,
    atomically, newTChan, readTChan, tryReadTChan, writeTChan)
import Control.Monad.STM

import Fco.Core.Util (whileDataM)


-- service, channel, message types

data Message a = Message a | QuitMsg 
  deriving (Eq, Ord, Show)

type Channel a = TChan (Message a)
type MsgHandler st a = st -> Message a -> IO (Maybe st)
type Listener st a = Channel a -> MsgHandler st a -> st -> IO ()

data HandledChannel st = forall a. HandledChannel (Channel a) (MsgHandler st a)
type MultiListener st = [HandledChannel st] -> st -> IO ()

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

multiListener :: MultiListener st
multiListener hChans = 
  whileDataM $ \state -> receiveChanAny state hChans


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


-- messaging functions

newChan :: IO (Channel a)
newChan = atomically newTChan

receiveChan :: Channel a -> IO (Message a)
receiveChan = atomically . readTChan

receiveChanAny :: st -> [HandledChannel st] -> IO (Maybe st)
receiveChanAny state hchans =
    join (atomically $ processHChans state hchans)
  where
      processHChans :: st -> [HandledChannel st] -> STM (IO (Maybe st))
      processHChans _ [] = retry
      processHChans state (HandledChannel chan handler : rest) = do
          msg <- tryReadTChan chan 
          case msg of
              Nothing -> processHChans state rest
              Just msg -> return (handler state msg)

sendChan :: Channel a -> Message a -> IO ()
sendChan chan msg = atomically $ writeTChan chan msg
