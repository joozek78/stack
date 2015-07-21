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

import Stack.Sig.Archive
import Stack.Sig.Config
import Stack.Sig.Defaults
import Stack.Sig.Doc
import Stack.Sig.GPG
import Stack.Sig.Types
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

list :: IO ()
list = do
    cfg <- readConfig
    home <- getHomeDirectory
    let archDir = home </> configDir </> archiveDir
    arch <- readArchive archDir
    verifyMappings
        cfg
        (archiveMappings arch)
        archDir
    putToDoc (archiveMappings arch)
