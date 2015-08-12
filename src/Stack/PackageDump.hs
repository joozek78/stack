{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Stack.PackageDump
    ( Line
    , eachSection
    , eachPair
    , DumpPackage (..)
    , conduitDumpPackage
    , ghcPkgDump
    , InstalledCache
    , InstalledCacheEntry (..)
    , newInstalledCache
    , loadInstalledCache
    , saveInstalledCache
    , addProfiling
    , addHaddock
    , sinkMatching
    , pruneDeps
    ) where

import           Control.Applicative
import           Control.Exception.Enclosed (tryIO)
import           Control.Monad (when, liftM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.Trans.Control
import           Data.Attoparsec.Args
import           Data.Attoparsec.Text as P
import           Data.Binary.VersionTagged
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Either (partitionEithers)
import qualified Data.Foldable as F
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Path
import           Path.IO (createTree)
import           Prelude -- Fix AMP warning
import           Stack.GhcPkg
import           Stack.Types
import           System.Directory (getDirectoryContents, doesFileExist)
import           System.Process.Read

-- | Cached information on whether package have profiling libraries and haddocks.
newtype InstalledCache = InstalledCache (IORef InstalledCacheInner)
newtype InstalledCacheInner = InstalledCacheInner (Map GhcPkgId InstalledCacheEntry)
    deriving (Binary, NFData)
instance BinarySchema InstalledCacheInner where
    -- Don't forget to update this if you change the datatype in any way!
    binarySchema _ = 1

-- | Cached information on whether a package has profiling libraries and haddocks.
data InstalledCacheEntry = InstalledCacheEntry
    { installedCacheProfiling :: !Bool
    , installedCacheHaddock :: !Bool }
    deriving (Eq, Generic)
instance Binary InstalledCacheEntry
instance NFData InstalledCacheEntry where
    rnf = genericRnf

-- | Call ghc-pkg dump with appropriate flags and stream to the given @Sink@, for a single database
ghcPkgDump
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
    => EnvOverride
    -> UseGHCJS
    -> Maybe (Path Abs Dir) -- ^ if Nothing, use global
    -> Sink ByteString IO a
    -> m a
ghcPkgDump menv useGHCJS mpkgDb sink = do
    F.mapM_ (createDatabase menv useGHCJS) mpkgDb -- TODO maybe use some retry logic instead?
    a <- sinkProcessStdout Nothing menv (ghcPkgExeName useGHCJS) args sink
    return a
  where
    args = concat
        [ case mpkgDb of
            Nothing -> ["--global", "--no-user-package-db"]
            Just pkgdb -> ["--user", "--no-user-package-db", "--package-db", toFilePath pkgdb]
        , ["dump", "--expand-pkgroot"]
        ]

-- | Create a new, empty @InstalledCache@
newInstalledCache :: MonadIO m => m InstalledCache
newInstalledCache = liftIO $ InstalledCache <$> newIORef (InstalledCacheInner Map.empty)

-- | Load a @InstalledCache@ from disk, swallowing any errors and returning an
-- empty cache.
loadInstalledCache :: MonadIO m => Path Abs File -> m InstalledCache
loadInstalledCache path = do
    m <- taggedDecodeOrLoad (toFilePath path) (return $ InstalledCacheInner Map.empty)
    liftIO $ fmap InstalledCache $ newIORef m

-- | Save a @InstalledCache@ to disk
saveInstalledCache :: MonadIO m => Path Abs File -> InstalledCache -> m ()
saveInstalledCache path (InstalledCache ref) = liftIO $ do
    createTree (parent path)
    readIORef ref >>= taggedEncodeFile (toFilePath path)

-- | Prune a list of possible packages down to those whose dependencies are met.
--
-- * id uniquely identifies an item
--
-- * There can be multiple items per name
pruneDeps
    :: (Ord name, Ord id)
    => (id -> name) -- ^ extract the name from an id
    -> (item -> id) -- ^ the id of an item
    -> (item -> [id]) -- ^ get the dependencies of an item
    -> (item -> item -> item) -- ^ choose the desired of two possible items
    -> [item] -- ^ input items
    -> Map name item
pruneDeps getName getId getDepends chooseBest =
      Map.fromList
    . (map $ \item -> (getName $ getId item, item))
    . loop Set.empty Set.empty []
  where
    loop foundIds usedNames foundItems dps =
        case partitionEithers $ map depsMet dps of
            ([], _) -> foundItems
            (s', dps') ->
                let foundIds' = Map.fromListWith chooseBest s'
                    foundIds'' = Set.fromList $ map getId $ Map.elems foundIds'
                    usedNames' = Map.keysSet foundIds'
                    foundItems' = Map.elems foundIds'
                 in loop
                        (Set.union foundIds foundIds'')
                        (Set.union usedNames usedNames')
                        (foundItems ++ foundItems')
                        (catMaybes dps')
      where
        depsMet dp
            | name `Set.member` usedNames = Right Nothing
            | all (`Set.member` foundIds) (getDepends dp) = Left (name, dp)
            | otherwise = Right $ Just dp
          where
            id' = getId dp
            name = getName id'

-- | Find the package IDs matching the given constraints with all dependencies installed.
-- Packages not mentioned in the provided @Map@ are allowed to be present too.
sinkMatching :: Monad m
             => Bool -- ^ require profiling?
             -> Bool -- ^ require haddock?
             -> Map PackageName Version -- ^ allowed versions
             -> Consumer (DumpPackage Bool Bool)
                         m
                         (Map PackageName (DumpPackage Bool Bool))
sinkMatching reqProfiling reqHaddock allowed = do
    dps <- CL.filter (\dp -> isAllowed (dpGhcPkgId dp) &&
                             (not reqProfiling || dpProfiling dp) &&
                             (not reqHaddock || dpHaddock dp))
       =$= CL.consume
    return $ pruneDeps
        (packageIdentifierName . ghcPkgIdPackageIdentifier)
        dpGhcPkgId
        dpDepends
        const -- Could consider a better comparison in the future
        dps
  where
    isAllowed gid =
        case Map.lookup name allowed of
            Just version' | version /= version' -> False
            _ -> True
      where
        PackageIdentifier name version = ghcPkgIdPackageIdentifier gid

-- | Add profiling information to the stream of @DumpPackage@s
addProfiling :: MonadIO m
             => InstalledCache
             -> Conduit (DumpPackage a b) m (DumpPackage Bool b)
addProfiling (InstalledCache ref) =
    CL.mapM go
  where
    go dp = liftIO $ do
        InstalledCacheInner m <- readIORef ref
        let gid = dpGhcPkgId dp
        p <- case Map.lookup gid m of
            Just installed -> return (installedCacheProfiling installed)
            Nothing | null (dpLibraries dp) -> return True
            Nothing -> do
                let loop [] = return False
                    loop (dir:dirs) = do
                        econtents <- tryIO $ getDirectoryContents dir
                        let contents = either (const []) id econtents
                        if or [isProfiling content lib
                              | content <- contents
                              , lib <- dpLibraries dp
                              ] && not (null contents)
                            then return True
                            else loop dirs
                loop $ dpLibDirs dp
        return dp { dpProfiling = p }

isProfiling :: FilePath -- ^ entry in directory
            -> ByteString -- ^ name of library
            -> Bool
isProfiling content lib =
    prefix `S.isPrefixOf` S8.pack content
  where
    prefix = S.concat ["lib", lib, "_p"]

-- | Add haddock information to the stream of @DumpPackage@s
addHaddock :: MonadIO m
           => InstalledCache
           -> Conduit (DumpPackage a b) m (DumpPackage a Bool)
addHaddock (InstalledCache ref) =
    CL.mapM go
  where
    go dp = liftIO $ do
        InstalledCacheInner m <- readIORef ref
        let gid = dpGhcPkgId dp
        h <- case Map.lookup gid m of
            Just installed -> return (installedCacheHaddock installed)
            Nothing | not (dpHasExposedModules dp) -> return True
            Nothing -> do
                let loop [] = return False
                    loop (ifc:ifcs) = do
                        exists <- doesFileExist (S8.unpack ifc)
                        if exists
                            then return True
                            else loop ifcs
                loop $ dpHaddockInterfaces dp
        return dp { dpHaddock = h }

-- | Dump information for a single package
data DumpPackage profiling haddock = DumpPackage
    { dpGhcPkgId :: !GhcPkgId
    , dpLibDirs :: ![FilePath]
    , dpLibraries :: ![ByteString]
    , dpHasExposedModules :: !Bool
    , dpDepends :: ![GhcPkgId]
    , dpHaddockInterfaces :: ![ByteString]
    , dpProfiling :: !profiling
    , dpHaddock :: !haddock
    }
    deriving (Show, Eq, Ord)

data PackageDumpException
    = MissingSingleField ByteString (Map ByteString [Line])
    | MismatchedId PackageName Version GhcPkgId
    | Couldn'tParseField ByteString [Line]
    deriving Typeable
instance Exception PackageDumpException
instance Show PackageDumpException where
    show (MissingSingleField name values) = unlines $ concat
        [ return $ concat
            [ "Expected single value for field name "
            , show name
            , " when parsing ghc-pkg dump output:"
            ]
        , map (\(k, v) -> "    " ++ show (k, v)) (Map.toList values)
        ]
    show (MismatchedId name version gid) =
        "Invalid id/name/version in ghc-pkg dump output: " ++
        show (name, version, gid)
    show (Couldn'tParseField name ls) =
        "Couldn't parse the field " ++ show name ++ " from lines: " ++ show ls

-- | Convert a stream of bytes into a stream of @DumpPackage@s
conduitDumpPackage :: MonadThrow m
                   => Conduit ByteString m (DumpPackage () ())
conduitDumpPackage = (=$= CL.catMaybes) $ eachSection $ do
    pairs <- eachPair (\k -> (k, ) <$> CL.consume) =$= CL.consume
    let m = Map.fromList pairs
    let parseS k =
            case Map.lookup k m of
                Just [v] -> return v
                _ -> throwM $ MissingSingleField k m
        -- Can't fail: if not found, same as an empty list. See:
        -- https://github.com/fpco/stack/issues/182
        parseM k =
            case Map.lookup k m of
                Just vs -> vs
                Nothing -> []

        parseDepend :: MonadThrow m => ByteString -> m (Maybe GhcPkgId)
        parseDepend "builtin_rts" = return Nothing
        parseDepend bs =
            liftM Just $ parseGhcPkgId bs'
          where
            (bs', _builtinRts) =
                case stripSuffixBS " builtin_rts" bs of
                    Nothing ->
                        case stripPrefixBS "builtin_rts " bs of
                            Nothing -> (bs, False)
                            Just x -> (x, True)
                    Just x -> (x, True)
    case Map.lookup "id" m of
        Just ["builtin_rts"] -> return Nothing
        _ -> do
            name <- parseS "name" >>= parsePackageName
            version <- parseS "version" >>= parseVersion
            ghcPkgId <- parseS "id" >>= parseGhcPkgId
            when (PackageIdentifier name version /= ghcPkgIdPackageIdentifier ghcPkgId)
                $ throwM $ MismatchedId name version ghcPkgId

            -- if a package has no modules, these won't exist
            let libDirKey = "library-dirs"
                libDirs = parseM libDirKey
                libraries = parseM "hs-libraries"
                exposedModules = parseM "exposed-modules"
                haddockInterfaces = parseM "haddock-interfaces"
            depends <- mapM parseDepend $ parseM "depends"

            libDirPaths <-
                case mapM (P.parseOnly (argsParser NoEscaping) . T.decodeUtf8) libDirs of
                    Left{} -> throwM (Couldn'tParseField libDirKey libDirs)
                    Right dirs -> return (concat dirs)

            return $ Just DumpPackage
                { dpGhcPkgId = ghcPkgId
                , dpLibDirs = libDirPaths
                , dpLibraries = S8.words $ S8.unwords libraries
                , dpHasExposedModules = not (null libraries || null exposedModules)
                , dpDepends = catMaybes (depends :: [Maybe GhcPkgId])
                , dpHaddockInterfaces = S8.words $ S8.unwords haddockInterfaces
                , dpProfiling = ()
                , dpHaddock = ()
                }

stripPrefixBS :: ByteString -> ByteString -> Maybe ByteString
stripPrefixBS x y
    | x `S.isPrefixOf` y = Just $ S.drop (S.length x) y
    | otherwise = Nothing

stripSuffixBS :: ByteString -> ByteString -> Maybe ByteString
stripSuffixBS x y
    | x `S.isSuffixOf` y = Just $ S.take (S.length y - S.length x) y
    | otherwise = Nothing

-- | A single line of input, not including line endings
type Line = ByteString

-- | Apply the given Sink to each section of output, broken by a single line containing ---
eachSection :: Monad m
            => Sink Line m a
            -> Conduit ByteString m a
eachSection inner =
    CL.map (S.filter (/= _cr)) =$= CB.lines =$= start
  where
    _cr = 13

    peekBS = await >>= maybe (return Nothing) (\bs ->
        if S.null bs
            then peekBS
            else leftover bs >> return (Just bs))

    start = peekBS >>= maybe (return ()) (const go)

    go = do
        x <- toConsumer $ takeWhileC (/= "---") =$= inner
        yield x
        CL.drop 1
        start

-- | Grab each key/value pair
eachPair :: Monad m
         => (ByteString -> Sink Line m a)
         -> Conduit Line m a
eachPair inner =
    start
  where
    start = await >>= maybe (return ()) start'

    _colon = 58
    _space = 32

    start' bs1 =
        toConsumer (valSrc =$= inner key) >>= yield >> start
      where
        (key, bs2) = S.break (== _colon) bs1
        (spaces, bs3) = S.span (== _space) $ S.drop 1 bs2
        indent = S.length key + 1 + S.length spaces

        valSrc
            | S.null bs3 = noIndent
            | otherwise = yield bs3 >> loopIndent indent

    noIndent = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just bs -> do
                let (spaces, val) = S.span (== _space) bs
                if S.length spaces == 0
                    then leftover val
                    else do
                        yield val
                        loopIndent (S.length spaces)

    loopIndent i =
        loop
      where
        loop = await >>= maybe (return ()) go

        go bs
            | S.length spaces == i && S.all (== _space) spaces =
                yield val >> loop
            | otherwise = leftover bs
          where
            (spaces, val) = S.splitAt i bs

-- | General purpose utility
takeWhileC :: Monad m => (a -> Bool) -> Conduit a m a
takeWhileC f =
    loop
  where
    loop = await >>= maybe (return ()) go

    go x
        | f x = yield x >> loop
        | otherwise = leftover x
