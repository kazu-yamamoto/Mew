module Util where

import System.Directory
import System.FilePath

-- symbolic link to real link

normalizePath :: FilePath -> IO FilePath
normalizePath path = expandHome path >>= canonicalizePath

expandHome :: FilePath -> IO FilePath
expandHome ('~':cs) = getHomeDirectory >>= return . (++ cs)
expandHome dir      = return dir
