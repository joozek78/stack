{-|
Module      : Main
Description : Haskell Package Signing Tool: Main
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig
       ( module Sig
       , sigCheckCmdName
       , sigCheckOpts
       , sigCmdName
       , sigInitCmdName
       , sigInitOpts
       , sigMappingCmdName
       , sigMappingOpts
       , sigSignCmdName
       , sigSignHackageCmdName
       , sigSignHackageOpts
       , sigSignSdistCmdName
       , sigSignSdistOpts
       , sigTrustCmdName
       , sigTrustOpts
       , sigUpdateCmdName
       , sigUpdateOpts
       )
       where

import Options.Applicative
import Stack.Sig.Archive as Sig
import Stack.Sig.Cabal as Sig
import Stack.Sig.Cabal.Parse as Sig
import Stack.Sig.Check as Sig
import Stack.Sig.Config as Sig
import Stack.Sig.Defaults as Sig
import Stack.Sig.Doc as Sig
import Stack.Sig.GPG as Sig
import Stack.Sig.Hackage as Sig
import Stack.Sig.Init as Sig
import Stack.Sig.Install as Sig
import Stack.Sig.List as Sig
import Stack.Sig.Mapping as Sig
import Stack.Sig.Sign as Sig
import Stack.Sig.Trust as Sig
import Stack.Sig.Types as Sig
import Stack.Sig.Update as Sig

-- | The command name for dealing with signatures.
sigCmdName :: String
sigCmdName = "sig"

-- | The command name for initializing.
sigInitCmdName :: String
sigInitCmdName = "init"

-- | The command name for updating signatures.
sigUpdateCmdName :: String
sigUpdateCmdName = "update"

-- | The command name for showing trust mappings.
sigMappingCmdName :: String
sigMappingCmdName = "mapping"

-- | The command name for trusting signatures.
sigTrustCmdName :: String
sigTrustCmdName = "trust"

-- | The command name for checking signatures.
sigCheckCmdName :: String
sigCheckCmdName = "check"

-- | The command name for signing packages.
sigSignCmdName :: String
sigSignCmdName = "sign"

-- | The command name for signing an sdist package file.
sigSignSdistCmdName :: String
sigSignSdistCmdName = "sdist"

-- | The command name for signing all your packages from hackage.org.
sigSignHackageCmdName :: String
sigSignHackageCmdName = "hackage"

-- | The URL of the running signature service to use (sig-service)
url :: Parser String
url = strOption
        (long "url" <>
         short 'u' <>
         metavar "URL" <>
         showDefault <>
         value "https://sig.commercialhaskell.org")

-- | Signature init options
sigInitOpts :: Parser String
sigInitOpts = helper <*> url

-- | Signature update options
sigUpdateOpts :: Parser String
sigUpdateOpts = helper <*> url

-- | Show mappings options
sigMappingOpts :: Parser ()
sigMappingOpts = helper <*> pure ()

-- | Signature trust options
sigTrustOpts :: Parser (String, String)
sigTrustOpts = helper <*>
    ((,) <$>
     argument str (metavar "FINGERPRINT") <*>
     argument str (metavar "EMAIL"))

-- | Signature sign (sdist) options
sigSignSdistOpts :: Parser (String, String)
sigSignSdistOpts = helper <*>
    ((,) <$> url <*>
     argument str (metavar "PATH"))

-- | Signature sign (hackage) options
sigSignHackageOpts :: Parser (String, String)
sigSignHackageOpts = helper <*>
    ((,) <$> url <*>
     argument str (metavar "USER"))

-- | Signature check options
sigCheckOpts :: Parser String
sigCheckOpts = helper <*>
    argument str (metavar "PACKAGE")
