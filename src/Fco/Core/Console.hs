{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Fco.Core.Console (conIn, conOutHandler, demo) where

import BasicPrelude
import qualified Data.Text as T

import Control.Monad.Extra (whileM)

import Fco.Core.Service (
    Channel, Listener, Message (..), MsgHandler, Service (..),
    defaultCtlHandler, defaultListener, dummyHandler, 
    newChan, receiveChan, sendChan, startService)


conIn :: Channel Text -> Listener () Text
conIn parentChan mailbox _ _ =
    whileM $ getLine >>= \case
        "bye" -> sendChan parentChan QuitMsg >> return False
        line -> sendChan parentChan (Message line) >> return True

conOutHandler :: MsgHandler () Text
conOutHandler _ (Message line) = putStrLn line >> (return $ Just ())
conOutHandler _ msg = defaultCtlHandler () msg

demo :: IO ()
demo = do
    conInChan <- newChan
    Service outChan _ <- startService defaultListener conOutHandler ()
    startService (conIn conInChan) dummyHandler ()
    whileM $ (receiveChan conInChan) >>= \case
        Message line -> (sendChan outChan $ Message line) >> return True
        QuitMsg -> (sendChan outChan QuitMsg) >> return False
        --_ -> return True


