module Main (main) where
import Data.Char
import Data.List
import System.FilePath.Posix
import System.Directory

import Lib

main :: IO ()
main = someFunc

getNextNumber path = do
    files <- listDirectory path
    let days = filter (\y -> isSuffixOf ".hs" y && isPrefixOf "Day" y ) files
    let highest = maximum $ map((\y -> read y::Int ) . filter  isDigit ) days
    return (highest + 1)