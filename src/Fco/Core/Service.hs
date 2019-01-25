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


data Service msg = Service (TChan msg) ThreadId

startService :: (TChan msg -> IO ()) -> IO (Service msg)
startService proc = do
    mailbox <- newChan
    pid <- forkIO $ proc mailbox
    return $ Service mailbox pid


newChan :: IO (TChan a)
newChan = atomically newTChan

receive :: TChan a -> IO a
receive = atomically . readTChan

send :: TChan a -> a -> IO ()
send chan msg = atomically $ writeTChan chan msg


conIn :: TChan Text -> TChan Text -> IO ()
conIn clientChan mailbox =
  whileM $ do
    line <- getLine
    case line of
      "bye" -> send clientChan "quit" >> return False
      _ -> send clientChan line >> return True

conOut :: TChan Text -> IO ()
conOut mailbox =
  whileM $ do
    line <- receive mailbox
    case line of
      "quit" -> return False
      _ -> putStrLn line >> return True

demo = do
  Service ch pid <- startService conOut
  startService $ conIn ch
