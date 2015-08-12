{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Functions for the GHC package database.

module Stack.GhcPkg
  (findGhcPkgId
  ,getGlobalDB
  ,EnvOverride
  ,envHelper
  ,createDatabase
  ,unregisterGhcPkgId
  ,getCabalPkgVer
  ,findGhcPkgHaddockHtml
  ,findGhcPkgDepends
  ,findTransitiveGhcPkgDepends
  ,listGhcPkgDbs
  ,ghcPkgExeName)
  where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as S8
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Path (Path, Abs, Dir, toFilePath, parent, parseAbsDir)
import           Path.IO (dirExists, createTree)
import           Prelude hiding (FilePath)
import           Stack.Constants
import           Stack.Types
import           System.Directory (canonicalizePath, doesDirectoryExist)
import           System.Process.Read

-- | Get the global package database
getGlobalDB :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
            => EnvOverride -> UseGHCJS -> m (Path Abs Dir)
getGlobalDB menv useGHCJS = do
    -- This seems like a strange way to get the global package database
    -- location, but I don't know of a better one
    bs <- ghcPkg menv useGHCJS [] ["list", "--global"] >>= either throwM return
    let fp = S8.unpack $ stripTrailingColon $ firstLine bs
    liftIO (canonicalizePath fp) >>= parseAbsDir
  where
    stripTrailingColon bs
        | S8.null bs = bs
        | S8.last bs == ':' = S8.init bs
        | otherwise = bs
    firstLine = S8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Run the ghc-pkg executable
ghcPkg :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
       => EnvOverride
       -> UseGHCJS
       -> [Path Abs Dir]
       -> [String]
       -> m (Either ReadProcessException S8.ByteString)
ghcPkg menv useGHCJS pkgDbs args = do
    eres <- go
    r <- case eres of
            Left _ -> do
                mapM_ (createDatabase menv useGHCJS) pkgDbs
                go
            Right _ -> return eres
    return r
  where
    go = tryProcessStdout Nothing menv (ghcPkgExeName useGHCJS) args'
    args' = packageDbFlags pkgDbs ++ args

-- | Create a package database in the given directory, if it doesn't exist.
createDatabase :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
               => EnvOverride -> UseGHCJS -> Path Abs Dir -> m ()
createDatabase menv useGHCJS db = do
    exists <- dirExists db
    unless exists $ do
        -- Creating the parent doesn't seem necessary, as ghc-pkg
        -- seems to be sufficiently smart. But I don't feel like
        -- finding out it isn't the hard way
        createTree (parent db)
        _ <- tryProcessStdout Nothing menv (ghcPkgExeName useGHCJS) ["init", toFilePath db]
        return ()

-- | Get the necessary ghc-pkg flags for setting up the given package database
packageDbFlags :: [Path Abs Dir] -> [String]
packageDbFlags pkgDbs =
          "--no-user-package-db"
        : map (\x -> ("--package-db=" ++ toFilePath x)) pkgDbs

-- | Get the value of a field of the package.
findGhcPkgField
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
    => EnvOverride
    -> UseGHCJS
    -> [Path Abs Dir] -- ^ package databases
    -> Text
    -> Text
    -> m (Maybe Text)
findGhcPkgField menv useGHCJS pkgDbs name field = do
    result <-
        ghcPkg
            menv
            useGHCJS
            pkgDbs
            ["field", "--simple-output", T.unpack name, T.unpack field]
    return $
        case result of
            Left{} -> Nothing
            Right lbs ->
                fmap (stripCR . T.decodeUtf8) $ listToMaybe $ S8.lines lbs
  where
    stripCR t = fromMaybe t (T.stripSuffix "\r" t)

-- | Get the id of the package e.g. @foo-0.0.0-9c293923c0685761dcff6f8c3ad8f8ec@.
findGhcPkgId :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
             => EnvOverride
             -> UseGHCJS
             -> [Path Abs Dir] -- ^ package databases
             -> PackageName
             -> m (Maybe GhcPkgId)
findGhcPkgId menv useGHCJS pkgDbs name = do
    mpid <- findGhcPkgField menv useGHCJS pkgDbs (packageNameText name) "id"
    case mpid of
        Just !pid -> return (parseGhcPkgId (T.encodeUtf8 pid))
        _ -> return Nothing

-- | Get the Haddock HTML documentation path of the package.
findGhcPkgHaddockHtml :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
                      => EnvOverride
                      -> UseGHCJS
                      -> [Path Abs Dir] -- ^ package databases
                      -> PackageIdentifier
                      -> m (Maybe (Path Abs Dir))
findGhcPkgHaddockHtml menv useGHCJS pkgDbs pkgId = do
    mpath <- findGhcPkgField menv useGHCJS pkgDbs (packageIdentifierText pkgId) "haddock-html"
    case mpath of
        Just !path0 -> do
            let path = T.unpack path0
            exists <- liftIO $ doesDirectoryExist path
            path' <- if exists
                then liftIO $ canonicalizePath path
                else return path
            return (parseAbsDir path')
        _ -> return Nothing

-- | Finds dependencies of package, and all their dependencies, etc.
findTransitiveGhcPkgDepends
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
    => EnvOverride
    -> UseGHCJS
    -> [Path Abs Dir] -- ^ package databases
    -> PackageIdentifier
    -> m (Set PackageIdentifier)
findTransitiveGhcPkgDepends menv useGHCJS pkgDbs pkgId0 =
    go pkgId0 Set.empty
  where
    go pkgId res = do
        deps <- findGhcPkgDepends menv useGHCJS pkgDbs pkgId
        loop (map ghcPkgIdPackageIdentifier deps) res
    loop [] res = return res
    loop (dep:deps) res = do
        if Set.member dep res
            then loop deps res
            else do
                res' <- go dep (Set.insert dep res)
                loop deps (Set.union res res')

-- | Get the dependencies of the package.
findGhcPkgDepends :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
                  => EnvOverride
                  -> UseGHCJS
                  -> [Path Abs Dir] -- ^ package databases
                  -> PackageIdentifier
                  -> m [GhcPkgId]
findGhcPkgDepends menv useGHCJS pkgDbs pkgId = do
    mdeps <- findGhcPkgField menv useGHCJS pkgDbs (packageIdentifierText pkgId) "depends"
    case mdeps of
        Just !deps -> return (mapMaybe (parseGhcPkgId . T.encodeUtf8) (T.words deps))
        _ -> return []

unregisterGhcPkgId :: (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m, MonadBaseControl IO m)
                    => EnvOverride
                    -> UseGHCJS
                    -> Path Abs Dir -- ^ package database
                    -> GhcPkgId
                    -> m ()
unregisterGhcPkgId menv useGHCJS pkgDb gid = do
    eres <- ghcPkg menv useGHCJS [pkgDb] args
    case eres of
        Left e -> $logWarn $ T.pack $ show e
        Right _ -> return ()
  where
    -- TODO ideally we'd tell ghc-pkg a GhcPkgId instead
    args = ["unregister", "--user", "--force", packageIdentifierString $ ghcPkgIdPackageIdentifier gid]

-- | Get the version of Cabal from the global package database.
getCabalPkgVer :: (MonadThrow m, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
               => EnvOverride -> UseGHCJS -> m Version
getCabalPkgVer menv useGHCJS =
    findGhcPkgId
        menv
        useGHCJS
        [] -- global DB
        cabalPackageName >>=
        maybe
            (throwM $ Couldn'tFindPkgId cabalPackageName)
            (return . packageIdentifierVersion . ghcPkgIdPackageIdentifier)

listGhcPkgDbs
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
    => EnvOverride -> UseGHCJS -> [Path Abs Dir] -> m [PackageIdentifier]
listGhcPkgDbs menv useGHCJS pkgDbs = do
    result <-
        ghcPkg
            menv
            useGHCJS
            pkgDbs
            ["list", "--simple-output"]
    return $
        case result of
            Left{} -> []
            Right lbs -> mapMaybe parsePackageIdentifier (S8.words lbs)

ghcPkgExeName :: UseGHCJS -> String
ghcPkgExeName UseGHCJS = "ghcjs-pkg"
ghcPkgExeName UseGHC = "ghc-pkg"
