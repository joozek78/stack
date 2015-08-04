{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Sig.Config where

{-|
Module      : Stack.Sig.Config
Description : Config File Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString as B
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Time ( formatTime, getCurrentTime )
import qualified Data.Yaml as Y
import           Stack.Sig.Defaults
import           Stack.Types.Sig (SigException(..), SigConfig(..), Signer(..))
import           System.Directory (renameFile, getHomeDirectory, doesFileExist,
                                   createDirectoryIfMissing)
import           System.FilePath ( (</>) )
import           Text.Email.Validate ( emailAddress )

#if MIN_VERSION_time(1,5,0)
import           Data.Time (defaultTimeLocale)
#else
import           System.Locale (defaultTimeLocale)
#endif

defaultSigners :: [Signer]
defaultSigners = [ Signer
                   { signerFingerprint = "ADD9 EB09 887A BE32 3EF7  F701 2141 7080 9616 E227"
                   , signerEmail = fromJust (emailAddress "chrisdone@gmail.com")
                   }
                 , Signer
                   { signerFingerprint = "5E6C 66B2 78BD B10A A636  57FA A048 E8C0 57E8 6876"
                   , signerEmail = fromJust (emailAddress "michael@snoyman.com")
                   }
                 , Signer
                   { signerFingerprint = "8C69 4F5B 6941 3F16 736F  E055 A9E6 D147 44A5 2A60"
                   , signerEmail = fromJust (emailAddress "tim@dysinger.net")
                   }]

defaultConfig :: SigConfig
defaultConfig = SigConfig defaultSigners

readConfig :: forall (m :: * -> *).
              (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
              => m SigConfig
readConfig = do
    home <- liftIO getHomeDirectory
    let cfg = home </> configDir </> configFile
    result <-
        Y.decodeEither <$>
        liftIO (B.readFile cfg)
    case result of
        Left msg -> throwM
                (ConfigParseException
                     (show msg))
        Right config -> return config

writeConfig :: forall (m :: * -> *).
              (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
            => SigConfig -> m ()
writeConfig cfg = do
    home <- liftIO getHomeDirectory
    let configPath = home </> configDir </> configFile
    oldExists <-
        liftIO (doesFileExist configPath)
    when
        oldExists
        (do time <- liftIO (getCurrentTime)
            liftIO
                (renameFile
                     configPath
                     (formatTime
                          defaultTimeLocale
                          (configPath <> "-%s")
                          time)))
    liftIO
        (createDirectoryIfMissing
             True
             (home </> configDir))
    liftIO
        (B.writeFile
             configPath
             (Y.encode
                  cfg
                  { configTrustedMappingSigners = ordNub
                        (defaultSigners ++ configTrustedMappingSigners cfg)
                  }))
    where ordNub l = go Set.empty l
          go _ [] = []
          go s (x:xs) = if x `Set.member` s
                  then go s xs
                  else x :
                       go (Set.insert x s) xs

writeConfigIfMissing :: forall (m :: * -> *).
                        (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
                     => SigConfig -> m ()
writeConfigIfMissing cfg = do
    home <- liftIO getHomeDirectory
    let configPath = home </> configDir </> configFile
    fileExists <-
        liftIO (doesFileExist configPath)
    unless fileExists (writeConfig cfg)

addSigner :: forall (m :: * -> *).
             (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
          => SigConfig -> Signer -> m ()
addSigner cfg signer = do
    writeConfig
        (cfg
         { configTrustedMappingSigners = (signer :
                                          (configTrustedMappingSigners cfg))
         })
