{-|
Module      : Stack.Sig.Trust
Description : Trusting Mappings
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Trust where

import Control.Exception (throwIO)
import Data.String (fromString)
import Stack.Sig.Config
import Stack.Sig.GPG
import Stack.Sig.Types
import Text.Email.Validate (validate)

trust :: String -> String -> IO ()
trust fingerprint email = do
    cfg <- readConfig
    case validate (fromString email) of
        Left e -> throwIO (InvalidEmailException e)
        Right email' -> do
            fullFP <-
                fullFingerprint
                    (FingerprintSample
                         (fromString fingerprint))
            addSigner cfg (Signer fullFP email')
