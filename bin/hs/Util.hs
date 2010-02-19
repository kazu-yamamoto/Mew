module Util where

import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO

-- symbolic link to real link

normalizePath :: FilePath -> IO FilePath
normalizePath path = expandHome path >>= canonicalizePath >>= return . dropTrailingPathSeparator
  where
    expandHome ('~':cs) = getHomeDirectory >>= return . (++ cs)
    expandHome dir      = return dir

help :: String -> IO ()
help message = getProgName >>= hPutStrLn stderr . (\prog -> "Usage: " ++ prog ++ " " ++ message)

splitArgOpt :: [String] -> ([String],[String])
splitArgOpt as = (filter isArg as, filter isOpt as)
  where
    isArg = not . isOpt
    isOpt = ("-" `isPrefixOf`)
