module Msg where

import Database.HDBC

type ID = String

data Msg = Msg { myid :: ID
               , path :: FilePath
               , paid :: ID
               , date :: String
               } deriving (Eq, Show)

toMsg :: [SqlValue] -> Msg
toMsg [a,b,c,d] = Msg { myid = fromSQL a
                      , path = fromSQL b
                      , paid = fromSQL c
                      , date = fromSQL d
                      }
  where
    fromSQL SqlNull = ""
    fromSQL x = fromSql x

toMsg _ = error "toMsg"

fromMsg :: Msg -> [SqlValue]
fromMsg msg = [ toSql (myid msg)
              , toSql (path msg)
              , toSql (paid msg)
              , toSql (date msg)
              ]
