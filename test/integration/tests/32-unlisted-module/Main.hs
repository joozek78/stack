import StackTest
import System.Directory
import System.Process --XXX

main :: IO ()
main = do
    --XXX this should also test an executable's main, since it looks like there may have been a regression that prevents that case (https://github.com/commercialhaskell/stack/commit/9a2bde432fc6d9b42258b5d9dd2a8e6ee5e42d3b#commitcomment-12724037)
    copyFile "src/Unlisted_OK.hs" "src/Unlisted.hs"
    stack ["build"]
    stack ["build"]
    copyFile "src/Unlisted_FAIL.hs" "src/Unlisted.hs"
    -- xxx <- readFile "src/Lib.hs"
    -- writeFile "src/Lib_.hs" (xxx ++ "\n--\n")
    -- renameFile "src/Lib_.hs" "src/Lib.hs"
    stackErr ["build"]
