{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Stack.Sig.Update
Description : Update Mappings & Signatures
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Update where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.Conduit as C (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Data.Monoid ((<>))
import           Data.Time (formatTime, getCurrentTime)
import           Network.HTTP.Conduit (Response(responseBody),
                                       withManager, http, parseUrl)
import           Stack.Sig.Defaults (configDir, archiveDir)
import           Stack.Sig.Types
import           System.Directory (renameDirectory,
                                   getTemporaryDirectory,
                                   getHomeDirectory,
                                   doesDirectoryExist)
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import           System.Process (readProcessWithExitCode)

#if MIN_VERSION_time(1,5,0)
import           Data.Time (defaultTimeLocale)
#else
import           System.Locale (defaultTimeLocale)
#endif

update :: forall (m :: * -> *).
          (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
       => String -> m ()
update url = do
    home <- liftIO getHomeDirectory
    temp <- liftIO getTemporaryDirectory
    let tempFile = temp </> "sig-archive.tar.gz"
        configPath = home </> configDir
        archivePath = configPath </> archiveDir
    request <-
        liftIO (parseUrl (url <> "/download/archive"))
    catch
        (withManager
             (\mgr ->
                   do res <- http request mgr
                      (responseBody res) C.$$+-
                          sinkFile tempFile))
        (\e ->
              throwM
                  (SigServiceException
                       (show (e :: SomeException))))
    oldExists <-
        liftIO (doesDirectoryExist archivePath)
    when
        oldExists
        (do time <- liftIO getCurrentTime
            liftIO
                (renameDirectory
                     archivePath
                     (formatTime
                          defaultTimeLocale
                          (archivePath <> "-%s")
                          time)))
    (code,_out,err) <-
        liftIO
            (readProcessWithExitCode
                 "tar"
                 ["xf", tempFile, "-C", configPath]
                 [])
    unless
        (code == ExitSuccess)
        (throwM (ArchiveUpdateException err))
