----------------------------------------------------------------
--
-- smew.hs
--

module Main where

import Control.Applicative
import Data.List
import Msg
import Param
import Search
import System.Environment
import Util

-- ./smew '<20100105.154842.110627375.kazu@iij.ad.jp>' ~/Mail/id.db #imap/kazu@localhost#imap/work.mew-dist

----------------------------------------------------------------

helpMessage :: String
helpMessage = "[-p|-c] id [db [dir]]"

options :: [String]
options = ["-p","-c"]

----------------------------------------------------------------

parseOpts :: [String] -> Maybe (Search [Msg])
parseOpts opts
  | opts == []       = Just searchFamily
  | unknown          = Nothing
  | "-p" `elem` opts = Just searchMe     -- to find a parent, specify pid
  | "-c" `elem` opts = Just searchChild
  | otherwise        = Nothing
  where
    unknown = (opts `union` options) \\ options /= []

parseArgs :: [String] -> Maybe (ID,FilePath,FilePath)
parseArgs []           = Nothing
parseArgs [mid]        = Just (mid, defaultDB, "")
parseArgs [mid,db]     = Just (mid, db, "")
parseArgs [mid,db,dir] = Just (mid, db, dir)
parseArgs _            = Nothing

----------------------------------------------------------------

main :: IO ()
main = do
    (args,opts) <- splitArgOpt <$> getArgs
    let mtri = parseArgs args
        mcmd = parseOpts opts
    exec mcmd mtri
  where
    exec (Just cmd) (Just (mid,db,dir)) = do
      db'  <- normalizePath db
      dir' <- normalizePath dir
      withDB db' (cmd mid dir') >>= printResults
    exec _ _ = help helpMessage
    printResults = mapM_ (putStrLn . path)
