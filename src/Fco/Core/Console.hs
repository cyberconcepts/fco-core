{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Core.Console where

import BasicPrelude
import qualified Data.Text as T

import Data.Binary (Binary)
import Control.Monad (forever)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Process, ProcessId,
    expect, send, spawnLocal)

import Fco.Core.Messaging


-- console service

data ConMsg = ConMsg Text
  deriving (Show, Generic, Typeable)
instance Binary ConMsg

setupConsole :: ProcessId -> Process ProcessId
setupConsole parent = do
    spawnLocal (conWriter parent) >>= return

conWriter :: ProcessId -> Process ()
conWriter parent = do
    pidR <- spawnLocal $ conReader parent
    forever $ expect >>= putStrLn

conReader :: ProcessId -> Process ()
conReader parent =
    forever $ do
      line <- getLine
      case line of
        "bye" -> send parent QuitMsg
        _ -> send parent $ ConMsg line

handleConMsg :: ProcessId -> ConMsg -> Process Bool
handleConMsg pid (ConMsg txt) = 
    send pid txt >> return True


