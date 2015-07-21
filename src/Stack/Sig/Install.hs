{-|
Module      : Stack.Sig.Install
Description : Haskell Package Signing Tool: Installing with Cabal
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Install where

import Stack.Sig.Cabal
import Stack.Sig.Check
import Stack.Sig.Doc

install :: [String] -> String -> IO ()
install extraArgs pkg = do
    check extraArgs pkg
    putHeader "Verifying Packages"
    cabalInstall extraArgs pkg
