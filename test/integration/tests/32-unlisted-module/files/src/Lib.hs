module Lib
    ( someFunc
    ) where

import Unlisted

someFunc :: IO ()
someFunc = do putStrLn "someFunc"
              foo

--
