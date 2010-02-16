module Main where

import Control.Applicative
import Control.Monad
import Mail
import Msg
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.Posix.Files
import Text.Regex.Posix
import System.Time
import Database.HDBC
import Database.HDBC.Sqlite3
import Param
import Util

type ModTime = String

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  db <- normalizePath defaultDB
  vmodtime <- getModTimeFromDB db
  let cnf = Config { ignoreRegex = "/casket$"
                   , msgRegex    = "/[0-9]+(\\.mew)?$"
                   , modfile = ".mew-mtime"
                   , modtime = vmodtime
                   }
  walkDirectory file cnf

data Config = Config { ignoreRegex :: String
                     , msgRegex :: String
                     , modfile :: String
                     , modtime :: ModTime
                     }

doit :: String -> IO ()
doit   dir = putStrLn dir

ignore :: String -> IO ()
ignore dir = putStrLn $ dir ++ " (ignored)"

skip :: String -> IO ()
skip dir = putStrLn $ dir ++ " (skipped)"

walkDirectory :: FilePath -> Config -> IO ()
walkDirectory dir cnf = do
    files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
    forM_ files $ \file -> do
      let file' = dir </> file
      st  <- getFileStatus file'
      st' <- getSymbolicLinkStatus file'
      switch file' cnf st st'

switch :: FilePath -> Config -> FileStatus -> FileStatus -> IO ()
switch file cnf st st'
  | isSymbolicLink st' = handleSymLink file st
  | isDirectory st     = handleDirectory file cnf st
  | isRegularFile st   = handleFile file cnf
  | otherwise          = return ()

handleSymLink :: FilePath -> FileStatus -> IO ()
handleSymLink file st = when (isDirectory st) $ ignore file

handleDirectory :: FilePath -> Config -> FileStatus -> IO ()
handleDirectory dir cnf st
  | dir =~ ignoreRegex cnf = ignore dir
  | otherwise        = do
    notModified <- check
    if notModified
       then do
         skip dir
         print $ linkCount st -- xxx
         when (linkCount st > 2) $ walkDirectory dir cnf
       else do
         doit dir
         walkDirectory dir cnf
  where
    check = do
      let mtimeFile = dir </> modfile cnf
      exist <- doesFileExist mtimeFile
      if exist
         then (modtime cnf >) <$> getModTime mtimeFile
         else return False

handleFile :: FilePath -> Config -> IO ()
handleFile _ _ = return ()
{-
handleFile file cnf
  | file =~ msgRegex cnf = (parseMail file <$> readFile file) >>= print
  | otherwise = return ()
-}

getModTime :: FilePath -> IO ModTime
getModTime file = toStr <$> getModificationTime file
  where
    toStr (TOD x y) = show x ++ show y

getModTimeFromDB :: String -> IO ModTime
getModTimeFromDB db = handleSqlError $ do
    conn <- connectSqlite3 db
    msgs <- func conn
    disconnect conn
    return $ date $ toMsg $ head msgs
  where
   func conn = quickQuery' conn "SELECT * FROM mew WHERE (id = ?);" [toSql "<mew-ctime>"]
