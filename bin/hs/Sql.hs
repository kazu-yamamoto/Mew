module Sql where

import Control.Applicative
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import Msg

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
