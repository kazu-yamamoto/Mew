module Search (Triple, Search, dispatch, searchMe, searchChild, searchFamily) where

import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Data.Ord
import Database.HDBC
import Database.HDBC.Sqlite3
import Message
import Sql
import System.FilePath

----------------------------------------------------------------

type Triple = (Id,FilePath,FilePath)
type Search = Connection -> Id -> FilePath -> IO [Message]

----------------------------------------------------------------

dispatch :: Search -> Triple -> IO [Message]
dispatch func (mid,db,dir) = handleSqlError $ do
    conn <- connectSqlite3 db
    msgs <- func conn mid dir
    disconnect conn
    return msgs

----------------------------------------------------------------

searchMe :: Search
searchMe conn mid dir = chooseOne dir <$> selectById conn mid

----------------------------------------------------------------

searchChild :: Search
searchChild conn mid dir = chooseOne dir <$> selectByParid conn [mid]

----------------------------------------------------------------

searchFamily :: Search
searchFamily conn mid dir = sortBy (comparing date) <$> findFamily
  where
    findFamily = findRoot conn mid mid >>= findDescendants conn dir

data ParentError = NoEntry | NoPid

getParid :: Connection -> Id -> IO (Either ParentError Id)
getParid conn mid = getPid <$> selectById conn mid
  where
    getPid []     = Left NoEntry
    getPid (e:_)
      | pid == "" = Left NoPid
      | otherwise = Right pid
      where
        pid = parid e

findRoot :: Connection -> Id -> Id -> IO Id
findRoot conn previd mid =
    getParid conn mid >>= either terminate (findRoot conn mid)
  where
    terminate NoEntry = return previd
    terminate NoPid   = return mid

type Hash = Map Id Message

findDescendants :: Connection -> FilePath -> Id -> IO [Message]
findDescendants conn dir rtid = do
    roots <- selectById conn rtid
    let root = head $ chooseOne dir roots
        mmap = Map.insert rtid root Map.empty
    findChildren ([rtid],mmap)
  where
    findChildren :: ([Id],Hash) -> IO [Message]
    findChildren (ids,hash) = do
        msgs <- selectByParid conn ids
        if msgs == [] then return (Map.elems hash)
                      else findChildren $ pushChildren hash msgs []
    pushChildren :: Hash -> [Message] -> [Id] -> ([Id],Hash)
    pushChildren hash [] ids = (ids, hash)
    pushChildren hash (m:ms) ids
        | Map.member mid hash = if (takeDirectory.path) m == dir
                                then let hash'  = Map.delete mid hash
                                         hash'' = Map.insert mid m hash'
                                     in pushChildren hash'' ms ids
                                else pushChildren hash ms ids
        | otherwise         = pushChildren (Map.insert mid m hash) ms (mid:ids)
      where
        mid = myid m

----------------------------------------------------------------
-- to express failure, the empty list is used

chooseOne :: FilePath -> [Message] -> [Message]
chooseOne _ [] = [] -- failure
chooseOne "" (m:_) = [m]
chooseOne dir msgs@(m:_)
  | null sames = [m]
  | otherwise  = [head sames]
  where
    sames = filter sameDir msgs
    sameDir x = (takeDirectory.path) x == dir
