{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Version (Version(..))
import Distribution.Package
       (PackageName(..), PackageIdentifier(..), packageVersion)
import Stack.Sig.Archive (readArchive)
import Stack.Sig.Cabal (cabalInstallDryRun, cabalFetch)
import Stack.Sig.Config (readConfig)
import Stack.Sig.Defaults (configDir, archiveDir)
import Stack.Sig.Doc (putHeader, putPkgOK)
import Stack.Sig.GPG (verifyPackage, verifyMappings)
import Stack.Sig.Types (Archive(..))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

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
    putHeader "Verifying Packages"
    pkgs <- cabalInstallDryRun extraArgs pkg
    forM_
        pkgs
        (\p ->
              do cabalFetch [] p
                 let (PackageName name) = pkgName p
                     version = intercalate
                             "."
                             (map
                                  show
                                  (versionBranch
                                       (packageVersion p)))
                     path = home </> ".cabal" </> "packages" </>
                         "hackage.haskell.org" </> name </> version </>
                         (name <> "-" <> version) <>
                         ".tar.gz"
                 verifyPackage arch p path
                 putPkgOK p)
