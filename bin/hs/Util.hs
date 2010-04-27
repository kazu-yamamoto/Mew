module Util where

import Data.List
import Control.Applicative
import System.Directory
import System.Environment
import System.FilePath
import System.IO

-- symbolic link to real link

normalizePath :: FilePath -> IO FilePath
normalizePath path = dropTrailingPathSeparator <$> (expandHome path >>= canonicalizePath)
  where
    expandHome ('~':cs) = (++ cs) <$> getHomeDirectory
    expandHome dir      = return dir

help :: String -> IO ()
help message = getProgName >>= hPutStrLn stderr . (\prog -> "Usage: " ++ prog ++ " " ++ message)

splitArgOpt :: [String] -> ([String],[String])
splitArgOpt as = (filter isArg as, filter isOpt as)
  where
    isArg = not . isOpt
    isOpt = ("-" `isPrefixOf`)
