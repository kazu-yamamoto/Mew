module Index (makeIndex) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Database.HDBC.Sqlite3
import Mail
import Msg
import Param
import Sql
import System.Directory
import System.FilePath ((</>))
import System.Time
import Text.Regex.Posix

----------------------------------------------------------------

data Control = Control { toFolder    :: FilePath -> FilePath
                       , ignoreRegex :: String
                       , msgRegex    :: String
                       , dirModFile  :: String
                       , dbModTime   :: Maybe Integer
                       , refDel      :: IORef Int
                       , refReg      :: IORef Int
                       , getmsg      :: Msg -> IO [Msg]
                       , addmsg      :: Msg -> IO ()
                       , delmsg      :: Msg -> IO ()
                       }

----------------------------------------------------------------

-- register modified time

makeIndex :: Bool -> Bool -> FilePath -> FilePath -> String -> IO (Int, Int)
makeIndex dryRun fullUpdate db dir re = withDB db $ \conn -> do
    ctl <- makeControl dryRun fullUpdate dir re conn
    walkDirectory dir ctl
    registered <- readIORef (refReg ctl)
    deleted    <- readIORef (refDel ctl)
    return (registered,deleted)

makeControl :: Bool -> Bool -> FilePath -> String -> Connection -> IO Control
makeControl dryRun fullUpdate dir re conn = do
    let dlen = length dir + 1
    refd <- newIORef 0
    refr <- newIORef 0
    dbMod <- dbModtime conn
    gstmt <- prepareGet conn
    astmt <- prepareAdd conn
    dstmt <- prepareDel conn
    return Control { toFolder    = drop dlen
                   , ignoreRegex = re
                   , msgRegex    = defaultMessageRegex
                   , dirModFile  = defaultDirModFile
                   , dbModTime   = if fullUpdate then Nothing else Just dbMod
                   , refDel      = refd
                   , refReg      = refr
                   , getmsg      = getMsg gstmt
                   , addmsg      = if dryRun then toBeAdded else addMsg astmt
                   , delmsg      = if dryRun then toBeDeled else delMsg dstmt
                   }
  where
    toBeDeled msg = putStrLn $ "  " ++ path msg ++ " (to be deleted)"
    toBeAdded msg = putStrLn $ "  " ++ path msg ++ " (to be added)"

----------------------------------------------------------------

doit :: String -> IO ()
doit dir = putStrLn dir

ignore :: String -> IO ()
ignore dir = putStrLn $ dir ++ " (ignored)"

skip :: String -> IO ()
skip dir = putStrLn $ dir ++ " (skipped)"

----------------------------------------------------------------

walkDirectory :: FilePath -> Control -> IO ()
walkDirectory dir ctl = do
    files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
    forM_ files $ \file -> do
      let file' = dir </> file
      isDir <- doesDirectoryExist file'
      -- xxx ignore symlink
      if isDir
         then handleDirectory file' ctl
         else handleFile      file' ctl

handleDirectory :: FilePath -> Control -> IO ()
handleDirectory dir ctl
  | dir =~ ignoreRegex ctl = ignore dir
  | otherwise = do
    modified <- isModified
    if modified
       then do
         doit dir
         walkDirectory dir ctl
       else do
         skip dir
         -- xxx check link count
         walkDirectory dir ctl
  where
    isModified = case dbModTime ctl of
      Nothing   -> return True
      Just dbmt -> do
        let mtimeFile = dir </> dirModFile ctl
        exist <- doesFileExist mtimeFile
        if exist
          then (dbmt <) <$> getModTime mtimeFile
          else return True

handleFile :: FilePath -> Control -> IO ()
handleFile file ctl
  | not (file =~ msgRegex ctl) = return ()
  | otherwise = do
      modified <- isModified
      when modified $ do
        mmsg <- parseMail (toFolder ctl file) <$> readFile file
        case mmsg of
          Nothing  -> return ()
          Just msg -> do
            register <- deleteMsgifMoved msg
            when register (registerMsg msg)
  where
    isModified = case dbModTime ctl of
      Nothing   -> return True
      Just dbmt -> (dbmt <) <$> getModTime file
    deleteMsgifMoved msg = case dbModTime ctl of
      Nothing   -> return True
      Just _    -> do
        msgs <- getmsg ctl msg
        msgs' <- filterM doesExist msgs
        let msgsToDel = msgs \\ msgs'
        mapM_ (delmsg ctl) msgsToDel
        let exist = any (\m -> path m == path msg) msgs'
        return (not exist)
    doesExist m = doesFileExist (path m)
    registerMsg msg = do
      addmsg ctl msg
      modifyIORef (refReg ctl) (+1)

----------------------------------------------------------------

getModTime :: FilePath -> IO Integer
getModTime file = modtimeToInteger <$> getModificationTime file

modtimeToInteger :: ClockTime -> Integer
modtimeToInteger (TOD x _) = x
