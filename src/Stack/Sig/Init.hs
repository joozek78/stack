{-|
Module      : Stack.Sig.Init
Description : Initialize with Default Config & Clone Sig-Archive
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Init where

import Stack.Sig.Config ( defaultConfig, writeConfigIfMissing )
import Stack.Sig.Update ( update )

initialize :: String -> IO ()
initialize url =
  do writeConfigIfMissing defaultConfig
     update url
