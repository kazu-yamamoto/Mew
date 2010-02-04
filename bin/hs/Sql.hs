module Sql where

import Control.Applicative
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import Message

selectById :: Connection -> Id -> IO [Message]
selectById conn mid = map toMsg <$> quickQuery' conn format params
  where
    format = "SELECT * FROM mew WHERE (mew.id = ?);"
    params = [toSql mid]

selectByParid :: Connection -> [Id] -> IO [Message]
selectByParid conn ids = map toMsg <$> quickQuery' conn format params
  where
    fmt    = replicate (length ids) "mew.parid = ?"
    fmt'   = intersperse " OR " fmt
    fmt''  = concat fmt'
    format = "SELECT * FROM mew WHERE (" ++ fmt'' ++ ");"
    params = map toSql ids
