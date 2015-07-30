{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Stack.Sig.Archive
Description : Package Archive Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Archive where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString as S
import           Data.List (isSuffixOf)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.Package (PackageIdentifier(..))
import           Stack.Sig.Cabal.Parse
import           Stack.Sig.Defaults
import           Stack.Sig.Mapping
import           Stack.Sig.Types
import           System.Directory.Parse (parseDirectory, filterDirectory)
import           System.FilePath (splitFileName, splitExtension, (</>))

-- | Read an archive from a directory.
readArchive :: forall (m :: * -> *).
               (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
            => FilePath -> m Archive
readArchive dir = do
    mappingFilepaths <-
        liftIO
            (parseDirectory
                 (dir </> mappingsDir)
                 (T.stripSuffix ".yaml" .
                  T.pack))
    mappings <-
        mapM
            (\(fp,name) ->
                  fmap
                      (name, )
                      (readMapping fp))
            mappingFilepaths
    signatures <-
        readSignatures
            (dir </> signaturesDir)
    return
        Archive
        { archiveMappings = M.fromList mappings
        , archiveSignatures = signatures
        }

-- | Read all signatures from the directory.
readSignatures :: forall (m :: * -> *).
                  (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
               => FilePath -> m (Map PackageIdentifier (Set Signature))
readSignatures dir = do
    packageNames <-
        liftIO (parseDirectory dir parsePackageName)
    fmap
        (M.fromList . concat)
        (mapM
             (\(pkgDir,name) ->
                   do versions <-
                          liftIO (parseDirectory pkgDir parseVersion)
                      versionSignatures <-
                          mapM
                              (\(verDir,ver) ->
                                    do signatures <-
                                           liftIO
                                               (filterDirectory
                                                    verDir
                                                    (isSuffixOf ".asc"))
                                       let signatures' = foldl
                                                   (\a b ->
                                                         let sig = snd
                                                                     (splitFileName
                                                                          (fst
                                                                               (splitExtension
                                                                                    b)))
                                                         in case splitOn "_" sig of
                                                                (fprint:_date:_sha) -> M.insert
                                                                        fprint
                                                                        b
                                                                        a
                                                                _ -> a)
                                                   M.empty
                                                   signatures
                                       fmap
                                           (PackageIdentifier name ver, )
                                           (forM
                                                (M.elems signatures')
                                                (fmap Signature .
                                                 liftIO . S.readFile)))
                              versions
                      return (map (second S.fromList) versionSignatures))
             packageNames)
