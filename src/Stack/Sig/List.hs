{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Stack.Sig.List
Description : List Mappings
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.List where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Stack.Sig.Archive
import Stack.Sig.Config
import Stack.Sig.Defaults
import Stack.Sig.Doc
import Stack.Sig.GPG
import Stack.Sig.Types
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

list :: forall (m :: * -> *).
        (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m)
     => m ()
list = do
    cfg <- readConfig
    home <- liftIO getHomeDirectory
    let archDir = home </> configDir </> archiveDir
    arch <- readArchive archDir
    verifyMappings
        cfg
        (archiveMappings arch)
        archDir
    putToDoc (archiveMappings arch)
