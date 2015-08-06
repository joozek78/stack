{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Stack.Sig.Check
Description : Check Package Signature(s)
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Check where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Data.Foldable (forM_)
import           Data.Monoid ((<>))
import           Data.Version (showVersion)
import           Stack.Sig.Archive
import           Stack.Sig.Cabal
import           Stack.Sig.Config
import           Stack.Sig.Defaults
import           Stack.Sig.Display
import           Stack.Sig.GPG
import           Stack.Types
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

check :: forall (m :: * -> *).
         (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m)
      => [String] -> String -> m ()
check extraArgs pkg = do
    cfg <- readConfig
    home <- liftIO getHomeDirectory
    let archDir = home </> configDir </> archiveDir
    arch <- readArchive archDir
    verifyMappings
        cfg
        (archiveMappings arch)
        archDir
    $logInfo "Verifying Packages:"
    -- pkgs <- cabalInstallDryRun extraArgs pkg
    -- forM_
    --     pkgs
    --     (\p ->
    --           do cabalFetch [] p
    --              let (PackageName name) = pkgName p
    --                  version = showVersion
    --                          (pkgVersion p)
    --                  path = home </> ".cabal" </> "packages" </>
    --                      "hackage.haskell.org" </> name </> version </>
    --                      (name <> "-" <> version) <>
    --                      ".tar.gz"
    --              $logInfo (displayPackageIdentifier p)
    --              verifyPackage arch p path)
