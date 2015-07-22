{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Stack.Sig.Sign
Description : Signing Packages
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Sign (sign, signTarBytes, signAll) where

import           Control.Applicative ((<$>))
import           Control.Monad (when, void)
import           Control.Monad.Catch (MonadMask, MonadThrow, bracket, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Lazy as L
import           Data.Foldable (forM_)
import           Data.List (isSuffixOf)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.UUID (toString)
import           Data.UUID.V4 (nextRandom)
import           Data.Version (showVersion)
import           Distribution.Package (PackageName(PackageName),
                                       PackageIdentifier(..))
import           Network.HTTP.Conduit (Response(..), RequestBody(..),
                                       Request(..), withManager,
                                       httpLbs, parseUrl)
import           Network.HTTP.Types (status200, methodPut)
import           Path
import           Path.IO
import           Stack.Sig.Cabal (cabalFetch, cabalFilePackageId,
                                  packagesFromIndex, getPackageTarballPath)
import qualified Stack.Sig.GPG as GPG
import           Stack.Sig.Hackage
import           Stack.Sig.Types
import           Stack.Types.Config
import           System.Directory (getDirectoryContents)
import           System.Process (readProcessWithExitCode)

withStackWorkTempDir :: forall (m :: * -> *).
        (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m, MonadBaseControl IO m)
     => (Path Rel Dir -> m ()) -> m ()
withStackWorkTempDir f = do
    uuid <- liftIO nextRandom
    uuidPath <-
        parseRelDir
            (toString uuid)
    let tempDir = workDirRel </>
            $(mkRelDir "tmp") </>
            uuidPath
    bracket
        (createTree tempDir)
        (const (removeTree tempDir))
        (const (f tempDir))

sign :: forall (m :: * -> *).
        (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m, MonadBaseControl IO m)
     => String -> FilePath -> m ()
sign url filePath = do
    withStackWorkTempDir
        (\tempDir ->
              (do liftIO
                      (void
                           (readProcessWithExitCode
                                "tar"
                                [ "xf"
                                , filePath
                                , "-C"
                                , toFilePath tempDir
                                , "--strip"
                                , "1"]
                                []))
                  -- TODO USE HASKELL'S `TAR` PACKAGE FOR EXTRACTING MIGHT WORK
                  -- BETTER ON SOME PLATFORMS THAN readProcessWithExitCode +
                  -- TAR.EXE
                  cabalFiles <-
                      (filter (isSuffixOf ".cabal")) <$>
                      (liftIO
                           (getDirectoryContents
                                (toFilePath tempDir)))
                  when
                      (null cabalFiles)
                      (error ("bogus hackage tarball " <> filePath))
                  cabalFile <-
                      parseRelFile
                          (head cabalFiles)
                  pkg <-
                      liftIO
                          (cabalFilePackageId
                               (toFilePath
                                    (tempDir </> cabalFile)))
                  signPackage url pkg filePath))

signTarBytes :: forall (m :: * -> *).
                (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m, MonadBaseControl IO m)
             => String -> FilePath -> L.ByteString -> m ()
signTarBytes url tarFile bs = do
    withStackWorkTempDir
        (\tempDir ->
              (do tarFilePath <- parseRelFile tarFile
                  let tempFilePath = tempDir </> tarFilePath
                      tempFile = toFilePath tempFilePath
                  liftIO (L.writeFile tempFile bs)
                  sign url tempFile))

signAll :: forall (m :: * -> *).
           (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m, MonadBaseControl IO m)
        => String -> String -> m ()
signAll url uname = do
    $logInfo "GPG signing all hackage packages"
    fromHackage <- packagesForMaintainer uname
    fromIndex <- packagesFromIndex
    forM_
        (filter
             (\x ->
                   (pkgName x) `elem`
                   (map pkgName fromHackage))
             fromIndex)
        (\pkg ->
              do liftIO
                     (cabalFetch
                          ["--no-dependencies"]
                          pkg)
                 filePath <-
                     liftIO (getPackageTarballPath pkg)
                 signPackage url pkg filePath)

signPackage :: forall (m :: * -> *).
               (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m, MonadBaseControl IO m)
            => String -> PackageIdentifier -> FilePath -> m ()
signPackage url pkg filePath = do
    $logInfo ("GPG signing " <> T.pack filePath)
    sig@(Signature signature) <- GPG.sign filePath
    let (PackageName name) = pkgName pkg
        version = showVersion
                (pkgVersion pkg)
    fingerprint <-
        GPG.verifyFile sig filePath >>=
        GPG.fullFingerprint
    req <-
        parseUrl
            (url <> "/upload/signature/" <> name <> "/" <> version <> "/" <>
             T.unpack (fingerprintSample fingerprint))
    let put = req
            { method = methodPut
            , requestBody = RequestBodyBS signature
            }
    res <-
        withManager
            (httpLbs put)
    when
        (responseStatus res /= status200)
        (throwM (GPGSignException "unable to sign & upload package"))
