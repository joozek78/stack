{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

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

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.String (fromString)
import Stack.Sig.Config
import Stack.Sig.GPG
import Stack.Sig.Types
import Text.Email.Validate (validate)

trust :: forall (m :: * -> *).
         (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
      => String -> String -> m ()
trust fingerprint email = do
    cfg <- readConfig
    case validate (fromString email) of
        Left e -> throwM (InvalidEmailException e)
        Right email' -> do
            fullFP <-
                fullFingerprint
                    (FingerprintSample
                         (fromString fingerprint))
            addSigner cfg (Signer fullFP email')
