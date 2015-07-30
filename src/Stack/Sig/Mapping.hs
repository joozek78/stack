{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Stack.Sig.Mapping
Description : Signer Mapping Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Mapping where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString as S
import           Data.Monoid ((<>))
import           Data.Yaml (decodeEither)
import           Stack.Sig.Types (SigException(..), Mapping)

-- | Try to read a mapping from the file. Throws an exception if it
-- fails.
readMapping :: forall (m :: * -> *).
               (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
            => FilePath -> m Mapping
readMapping fp = do
    mm <- liftIO (S.readFile fp)
    case decodeEither mm of
        Right m -> return m
        Left e -> throwM
                (MappingParseException
                     ("Unable to parse mapping from mapping file " <> fp <> ": " <>
                      e))
