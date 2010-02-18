{-# LANGUAGE BangPatterns #-}

module Index (makeIndex) where

-- import Data.ByteString.Lazy.Char8 (unpack, hGet)

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
import System.IO
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
                       , processDir  :: String -> IO ()
                       , ignoreDir   :: String -> IO ()
                       , skipDir     :: String -> IO ()
                       }

defaultCtl :: Control
defaultCtl = Control { toFolder    = undefined
                     , ignoreRegex = defaultIgnoreRegex
                     , msgRegex    = defaultMessageRegex
                     , dirModFile  = defaultDirModFile
                     , dbModTime   = Nothing
                     , refDel      = undefined
                     , refReg      = undefined
                     , getmsg      = undefined
                     , addmsg      = toBeAdded
                     , delmsg      = toBeDeled
                     , processDir  = process
                     , ignoreDir   = ignore
                     , skipDir     = skip
                     }

----------------------------------------------------------------

process :: String -> IO ()
process dir = putStrLn dir

ignore :: String -> IO ()
ignore dir = putStrLn $ dir ++ " (ignored)"

skip :: String -> IO ()
skip dir = putStrLn $ dir ++ " (skipped)"

toBeDeled :: Msg -> IO ()
toBeDeled msg = putStrLn $ "  " ++ path msg ++ " (to be deleted)"

toBeAdded :: Msg -> IO ()
toBeAdded msg = putStrLn $ "  " ++ path msg ++ " (to be added)"

----------------------------------------------------------------

makeIndex :: Bool -> Bool -> FilePath -> FilePath -> String -> IO (Int, Int)
makeIndex dryRun fullUpdate db dir re = withDB db $ \conn -> do
    stime <- modtimeToInteger <$> getClockTime
    let ctl0 = defaultCtl
    ctl1 <- makeControl1 ctl0 dir re
    ctl2 <- makeControl2 ctl1 fullUpdate conn
    ctl3 <- makeControl3 ctl2 dryRun conn
    walkDirectory dir ctl3
    unless dryRun $ setDBModtime conn stime
    registered <- readIORef (refReg ctl3)
    deleted    <- readIORef (refDel ctl3)
    return (registered,deleted)

makeControl1 :: Control -> FilePath -> String -> IO Control
makeControl1 ctl dir re = do
    let dlen = length dir + 1
    refd <- newIORef 0
    refr <- newIORef 0
    return ctl { toFolder    = drop dlen
               , ignoreRegex = re
               , refDel      = refd
               , refReg      = refr
               }
-- Fullupdate
makeControl2 :: Control -> Bool -> Connection -> IO Control
makeControl2 ctl True  _    = return ctl
makeControl2 ctl False conn = do
    dbMod <- dbModtime conn
    return ctl { dbModTime = dbMod }

-- Dryrun
makeControl3 :: Control -> Bool -> Connection -> IO Control
makeControl3 ctl True conn = do
    gstmt <- prepareGet conn
    return ctl { getmsg = getMsg gstmt }
makeControl3 ctl False conn = do
    gstmt <- prepareGet conn
    astmt <- prepareAdd conn
    dstmt <- prepareDel conn
    return ctl { getmsg = getMsg gstmt
               , addmsg = addMsg astmt
               , delmsg = delMsg dstmt
               }

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
  | dir =~ ignoreRegex ctl = ignoreDir ctl dir
  | otherwise = do
    modified <- isModified
    if modified
       then do
         processDir ctl dir
         walkDirectory dir ctl
       else do
         skipDir ctl dir
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
        -- HERE HERE HERE
{-
        mmsg <- withBinaryFile file ReadMode $ \h -> do
          cs <- unpack <$> hGet h 4096
          let !fs = parseMail (toFolder ctl file) cs
          return fs
-}
        mmsg <- withBinaryFile file ReadMode $ \h -> do
          !fs <- parseMail (toFolder ctl file) <$> hGetContents h
          return fs
        case mmsg of
          Nothing  -> return ()
          Just msg -> do
            {- xxx dbModTime == Nothing ->  don't delete process!!!!
            register <- deleteMsgifMoved msg
            when register (registerMsg msg)
            -}
            registerMsg msg
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
