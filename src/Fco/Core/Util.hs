{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Fco.Core.Util (withData) where

import BasicPrelude


withData :: Monad m => (s -> m (Maybe s)) -> s -> m ()
withData act state =
    act state >>= \case
      Nothing -> return ()
      Just newState -> withData act newState
