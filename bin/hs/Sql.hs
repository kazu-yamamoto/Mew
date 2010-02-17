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
    res <- cmd conn
    disconnect conn
    return res

----------------------------------------------------------------

selectByID :: Connection -> ID -> IO [Msg]
selectByID conn mid = map toMsg <$> quickQuery' conn format params
  where
    format = "SELECT * FROM mew WHERE (mew.id = ?);"
    params = [toSql mid]

selectByPaID :: Connection -> [ID] -> IO [Msg]
selectByPaID conn ids = map toMsg <$> quickQuery' conn format params
  where
    fmt    = replicate (length ids) "mew.parid = ?"
    fmt'   = intersperse " OR " fmt
    fmt''  = concat fmt'
    format = "SELECT * FROM mew WHERE (" ++ fmt'' ++ ");"
    params = map toSql ids

----------------------------------------------------------------

prepareGet :: Connection -> IO Statement
prepareGet conn = prepare conn "SELECT * FROM mew WHERE (id = ?);"

prepareDel :: Connection -> IO Statement
prepareDel conn = prepare conn "DELETE FROM mew WHERE (id = ?) AND (path = ?);"

prepareAdd :: Connection -> IO Statement
prepareAdd conn = prepare conn "INSERT INTO mew VALUES (?, ?, ?, ?);"

getMsg :: Statement -> Msg -> IO [Msg]
getMsg stmt msg = map toMsg <$> (execute stmt params *> fetchAllRows stmt)
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

----------------------------------------------------------------

dbModtime :: Connection -> IO Integer
dbModtime conn = toI <$> quickQuery' conn format params
  where
   toI = read . date . toMsg . head
   format = "SELECT * FROM mew WHERE (id = ?);"
   params = [toSql "<mew-ctime>"]
