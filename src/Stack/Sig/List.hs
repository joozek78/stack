{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Data.Foldable
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Stack.Sig.Archive
import           Stack.Sig.Config
import           Stack.Sig.Defaults
import           Stack.Sig.Display
import           Stack.Sig.GPG
import           Stack.Sig.Types
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

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
    $logInfo "Mappings:"
    forM_
        (M.toList (archiveMappings arch))
        (\(mappingName,mapping) ->
              do $logInfo (T.pack ("  " <> T.unpack mappingName))
                 forM_
                     (M.toList (mappingSigners mapping))
                     (\(signer,pkgs) ->
                           do $logInfo ("    " <> displaySigner signer)
                              forM_
                                  pkgs
                                  ($logInfo .
                                   (<>) "      " .
                                   displayPackageName)))
