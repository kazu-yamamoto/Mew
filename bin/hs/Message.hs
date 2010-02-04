module Message where

import System.FilePath
import Database.HDBC

type Id = String

data Message = Message { myid  :: Id
                       , path  :: FilePath
                       , parid :: Id
                       , date  :: String
                       } deriving (Eq, Show)

toMsg :: [SqlValue] -> Message
toMsg [a,b,c,d] = Message { myid  = fromSQL a
                          , path  = fromSQL b
                          , parid = fromSQL c
                          , date  = fromSQL d
                          }
  where
    fromSQL SqlNull = ""
    fromSQL x = fromSql x

toMsg _ = error "toMsg"

fromMsg :: Message -> [SqlValue]
fromMsg msg = [ toSql (myid msg)
              , toSql (path msg)
              , toSql (parid msg)
              , toSql (date msg)
              ]
