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

import           Control.Exception (throwIO)
import qualified Data.ByteString as S
import           Data.Monoid ((<>))
import           Data.Yaml (decodeEither)
import           Stack.Sig.Types (SigException(..), Mapping)

-- | Try to read a mapping from the file. Throws an exception if it
-- fails.
readMapping :: FilePath -> IO Mapping
readMapping fp =
  do mm <- S.readFile fp
     case decodeEither mm of
       Right m -> return m
       Left e ->
         throwIO (MappingParseException
                    ("Unable to parse mapping from mapping file " <> fp <> ": " <>
                     e))
