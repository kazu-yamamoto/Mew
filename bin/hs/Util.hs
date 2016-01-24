{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import Control.Applicative
import Control.Exception as E
import Data.List
import Param
import System.EasyFile
import System.Environment
import System.IO

-- symbolic link to real link

normalizePath :: FilePath -> IO FilePath
normalizePath path = dropTrailingPathSeparator <$> (expandHome path >>= realPath)
  where
    realPath sym = canonicalizePath sym `E.catch` \(_::SomeException) -> return sym
    expandHome ""       = expandHome defaultMailDir
    expandHome ('~':cs) = do
        mhome <- getHomeDirectory2
        case mhome of
            Just home -> return $ home ++ cs
            Nothing   -> (++ cs) <$> getHomeDirectory
    expandHome dir
      | isRelative dir = expandHome (defaultMailDir </> dir)
      | otherwise      = return dir

help :: String -> IO ()
help message = getProgName >>= hPutStrLn stderr . (\prog -> "Usage: " ++ prog ++ " " ++ message)

splitArgOpt :: [String] -> ([String],[String])
splitArgOpt as = (filter isArg as, filter isOpt as)
  where
    isArg = not . isOpt
    isOpt = ("-" `isPrefixOf`)
