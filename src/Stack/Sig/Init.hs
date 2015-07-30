{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Stack.Sig.Init
Description : Initialize with Default Config & Clone Sig-Archive
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Init where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Stack.Sig.Config
import Stack.Sig.Update

initialize :: forall (m :: * -> *).
              (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
           => String -> m ()
initialize url = do
    writeConfigIfMissing defaultConfig
    update url
