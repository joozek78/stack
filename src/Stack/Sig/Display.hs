{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Stack.Sig.Display
Description : Formats data for display
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Display where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import           Data.Version (showVersion)
-- import           Distribution.Text (display)
import           Stack.Types
import           Text.Email.Validate (toByteString)

displayPackageIdentifier :: PackageIdentifier -> Text
displayPackageIdentifier = packageIdentifierText

displayPackageName :: PackageName -> Text
displayPackageName = T.pack . show

displaySigner :: Signer -> Text
displaySigner Signer{..} = (T.decodeUtf8 . toByteString) signerEmail <>
    " " <>
    displayFingerprint signerFingerprint

displayFingerprint :: FingerprintSample -> Text
displayFingerprint FingerprintSample{..} = fingerprintSample
