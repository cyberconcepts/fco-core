{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Fco.Core.Util (whileDataM) where

import BasicPrelude


whileDataM :: Monad m => (s -> m (Maybe s)) -> s -> m ()
whileDataM act state =
    act state >>= \case
      Nothing -> return ()
      Just newState -> whileDataM act newState
