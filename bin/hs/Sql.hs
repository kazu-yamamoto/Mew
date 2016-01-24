module Sql where

import Control.Applicative
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import Msg

----------------------------------------------------------------

withDB :: FilePath -> (Connection -> IO a) -> IO a
withDB db cmd = handleSqlError $ do
    conn <- connectSqlite3 db
    res  <- withTransaction conn cmd
    disconnect conn
    return res

----------------------------------------------------------------

createDB :: Connection -> IO ()
createDB conn = () <$ run conn format []
  where
    format = "CREATE TABLE IF NOT EXISTS mew (id TEXT, path TEXT, parid TEXT, date TEXT);"

indexDB :: Connection -> IO ()
indexDB conn = do
   _ <- run conn "CREATE INDEX IF NOT EXISTS mew_id ON mew (id);" []
   _ <- run conn "REINDEX mew_id;" []
   _ <- run conn "CREATE INDEX IF NOT EXISTS mew_parid ON mew (parid);" []
   _ <- run conn "REINDEX mew_parid;" []
   return ()

----------------------------------------------------------------

getByID :: Connection -> ID -> IO [Msg]
getByID conn mid = map toMsg <$> quickQuery' conn format params
  where
    format = "SELECT * FROM mew WHERE (id = ?);"
    params = [toSql mid]

getByPaID :: Connection -> [ID] -> IO [Msg]
getByPaID conn ids = map toMsg <$> quickQuery' conn format params
  where
    fmt    = replicate (length ids) "parid = ?"
    fmt'   = intersperse " OR " fmt
    fmt''  = concat fmt'
    format = "SELECT * FROM mew WHERE (" ++ fmt'' ++ ");"
    params = map toSql ids

delByID :: Connection -> ID -> IO ()
delByID conn mid = () <$ run conn format params
  where
    format = "DELETE FROM mew WHERE (id = ?);"
    params = [toSql mid]

addByID :: Connection -> ID -> String -> IO ()
addByID conn mid dat = () <$ run conn format params
  where
    format = "INSERT INTO mew VALUES (?, ?, ?, ?);"
    params = [toSql mid, toSql "", toSql "", toSql dat]

----------------------------------------------------------------

idForModtime :: String
idForModtime = "<mew-ctime>"

dbModtime :: Connection -> IO (Maybe Integer)
dbModtime conn = do
    msgs <- getByID conn idForModtime
    if null msgs
       then return Nothing
       else return $ toI msgs
  where
    toI = Just . read . date . head

setDBModtime :: Connection -> Integer -> IO ()
setDBModtime conn mt = do
     let mts = show mt
     mint <- dbModtime conn
     case mint of
       Nothing  -> return ()
       Just _   -> delByID conn idForModtime
     addByID conn idForModtime mts

----------------------------------------------------------------

prepareGet :: Connection -> IO Statement
prepareGet conn = prepare conn "SELECT * FROM mew WHERE (id = ?);"

prepareDel :: Connection -> IO Statement
prepareDel conn = prepare conn "DELETE FROM mew WHERE (id = ?) AND (path = ?);"

prepareAdd :: Connection -> IO Statement
prepareAdd conn = prepare conn "INSERT INTO mew VALUES (?, ?, ?, ?);"

getMsg :: Statement -> Msg -> IO [Msg]
getMsg stmt msg = map toMsg <$> (execute stmt params *> fetchAllRows' stmt)
  where
    params = [toSql (myid msg)]

delMsg :: Statement -> Msg -> IO ()
delMsg stmt msg = () <$ execute stmt params
  where
    params = [toSql (myid msg), toSql (path msg)]

addMsg :: Statement -> Msg -> IO ()
addMsg stmt msg = () <$ execute stmt params
  where
    params = fromMsg msg
