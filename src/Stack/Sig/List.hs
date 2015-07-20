{-# LANGUAGE NoImplicitPrelude #-}

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

import BasePrelude
import Stack.Sig.Archive ( readArchive )
import Stack.Sig.Config ( readConfig )
import Stack.Sig.Defaults ( configDir, archiveDir )
import Stack.Sig.Doc ( putToDoc )
import Stack.Sig.GPG ( verifyMappings )
import Stack.Sig.Types ( Archive(archiveMappings) )
import System.Directory ( getHomeDirectory )
import System.FilePath ( (</>) )

list :: IO ()
list =
  do cfg <- readConfig
     home <- getHomeDirectory
     let archDir = home </> configDir </> archiveDir
     arch <- readArchive archDir
     verifyMappings cfg
                    (archiveMappings arch)
                    archDir
     putToDoc (archiveMappings arch)
