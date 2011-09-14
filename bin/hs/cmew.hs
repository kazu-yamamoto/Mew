----------------------------------------------------------------
--
-- cmew.hs
--

module Main where

import Control.Applicative
import Data.List
import Index
import Param
import System.Environment
import Util

----------------------------------------------------------------

helpMessage :: String
helpMessage = "[-n] [-f] [db [dir [dignore_regex [target_folder]]]]"

options :: [String]
options = ["-n","-f"]

parseOpts :: [String] -> Maybe (Bool,Bool)
parseOpts opts
  | unknown   = Nothing
  | otherwise = Just (dryRun, fullUpdate)
  where
    unknown = (opts `union` options) \\ options /= []
    dryRun     = "-n" `elem` opts
    fullUpdate = "-f" `elem` opts

parseArgs :: [String] -> Maybe (FilePath,FilePath,Regexp,Maybe FilePath)
parseArgs []                 = Just (defaultDB,defaultMailDir,defaultIgnoreRegex,defaultTarget)
parseArgs [db]               = Just (db,defaultMailDir,defaultIgnoreRegex,defaultTarget)
parseArgs [db,dir]           = Just (db,dir,defaultIgnoreRegex,defaultTarget)
parseArgs [db,dir,re]        = Just (db,dir,re,defaultTarget)
parseArgs [db,dir,re,subdir] = Just (db,dir,re,Just subdir)
parseArgs _                  = Nothing

----------------------------------------------------------------

main :: IO ()
main = do
    (args,opts) <- splitArgOpt <$> getArgs
    let mtri = parseArgs args
        mopt = parseOpts opts
    exec mopt mtri
  where
    exec (Just (dryRun,fullUpdate)) (Just (db,dir,re,target)) = do
      db' <- normalizePath db
      dir' <- normalizePath dir
      makeIndex dryRun fullUpdate db' dir' re target >>= printResults
    exec _ _ = help helpMessage
    printResults (reg,del) = putStrLn $ "Registered: " ++ show reg ++  ", deleted: " ++ show del
