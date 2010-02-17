----------------------------------------------------------------
--
-- cmew.hs
--

module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Index
import Param
import System.Environment
import Util

----------------------------------------------------------------

helpMessage :: String
helpMessage = "[-n] [-f] [db [dir [dignore_regex [target_folder]]]]"

parseOpts :: [String] -> (Bool,Bool)
parseOpts opts = let dryRun     = "-n" `elem` opts
                     fullUpdate = "-f" `elem` opts
                 in (dryRun, fullUpdate)

parseArgs :: [String] -> Maybe (FilePath,FilePath,String)
parseArgs []          = Just (defaultDB,defaultMailDir,defaultMessageRegex)
parseArgs [db]        = Just (db,defaultMailDir,defaultMessageRegex)
parseArgs [db,dir]    = Just (db,dir,defaultMessageRegex)
parseArgs [db,dir,re] = Just (db,dir,re)
parseArgs _           = Nothing

----------------------------------------------------------------

main :: IO ()
main = do
    (args,opts) <- splitArgOpt <$> getArgs
    let mtri = parseArgs args
        mopt = parseOpts opts
    exec mopt mtri
  where
    exec (dryRun,fullUpdate) (Just (db,dir,re)) = do
      db' <- normalizePath db
      dir' <- normalizePath dir
      makeIndex dryRun fullUpdate db' dir' re >>= printResults
    exec _ _ = help helpMessage
    printResults (reg,del) = putStrLn $ "Registered: " ++ show reg ++  ", deleted: " ++ show del
