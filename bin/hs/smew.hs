----------------------------------------------------------------
--
-- smew.hs
--

module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Msg
import Param
import Search
import System
import System.FilePath
import System.IO
import Util

-- ./smew '<20100105.154842.110627375.kazu@iij.ad.jp>' ~/Mail/id.db #imap/kazu@localhost#imap/work.mew-dist

----------------------------------------------------------------

helpMessage :: String
helpMessage = "[-p|-c] id [db [dir]]"

help :: IO ()
help = getProgName >>= hPutStrLn stderr . (++ " " ++ helpMessage)

----------------------------------------------------------------

parseOpts :: [[Char]] -> Maybe Search
parseOpts opts
    | opts == []       = Just searchFamily
    | "-p" `elem` opts = Just searchMe     -- to find a parent, specify pid
    | "-c" `elem` opts = Just searchChild
    | otherwise        = Nothing

parseKeys :: [String] -> IO (Maybe Triple)
parseKeys []           = return Nothing
parseKeys [mid]        = toTriple mid defaultDB ""
parseKeys [mid,db]     = toTriple mid db ""
parseKeys [mid,db,dir] = toTriple mid db dir
parseKeys _            = return Nothing

toTriple :: ID -> FilePath -> FilePath -> IO (Maybe Triple)
toTriple mid db dir = do
    db'  <- nomalizePath db
    let dir' = dropTrailingPathSeparator dir
    return $ Just (mid,db',dir')

----------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let opts = filter isOption args
        keys = filter (not.isOption) args
        cmd  = parseOpts opts
    exec cmd <$> parseKeys keys >>=
      maybe help (>>= printResults)
  where
    isOption = ("-" `isPrefixOf`)
    exec (Just func) (Just ckey) = Just (dispatch func ckey)
    exec _           _           = Nothing
    printResults = mapM_ (putStrLn . path)
