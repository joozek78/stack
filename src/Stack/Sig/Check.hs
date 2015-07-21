{-# LANGUAGE OverloadedStrings #-}

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

check :: [String] -> String -> IO ()
check extraArgs pkg = do
    cfg <- readConfig
    home <- getHomeDirectory
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
