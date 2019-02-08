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

conIn :: Channel Text -> Listener () Text
conIn parentChan mailbox _ _ =
    whileM $ getLine >>= \case
        "bye" -> sendChan parentChan QuitMsg >> return False
        line -> sendChan parentChan (Message line) >> return True

conOutHandler :: MsgHandler () Text
conOutHandler _ (Message line) = putStrLn line >> (return $ Just ())
conOutHandler _ msg = defaultCtlHandler () msg

demo = do
    --conSrv <- startService defaultListener conOutHandler ()
    Service outChan _ <- startService defaultListener conOutHandler ()
    startService (conIn outChan) dummyHandler ()


-- low-level messaging definitions

data HandledChannel = forall a. HandledChannel (Channel a) (Message a -> IO Bool)

newChan :: IO (TChan a)
newChan = atomically newTChan

receiveChan :: TChan msg -> IO msg
receiveChan = atomically . readTChan

receiveChanAny :: [HandledChannel] -> IO Bool
receiveChanAny hchans =
    join (atomically $ processHChans hchans)
  where
      processHChans :: [HandledChannel] -> STM (IO Bool)
      processHChans [] = retry
      processHChans (HandledChannel chan handler : rest) = do
          msg <- tryReadTChan chan 
          case msg of
              Nothing -> processHChans rest
              Just msg -> return (handler msg)

sendChan :: TChan msg -> msg -> IO ()
sendChan chan msg = atomically $ writeTChan chan msg
