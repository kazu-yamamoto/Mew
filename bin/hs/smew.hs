----------------------------------------------------------------
--
-- smew.hs
--

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System
import System.IO
import System.FilePath
import System.Directory
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.ByteString.Char8 as BS

----------------------------------------------------------------

helpMessage = "[-p|-c] id [db [dir]]"

help :: IO ()
help = do prog <- getProgName
          hPutStrLn stderr $ prog ++ " " ++ helpMessage

----------------------------------------------------------------
-- smew '<20081014.105255.216914257.kazu@iij.ad.jp>' ~/Mail/id.db #imap/kazu@localhost#imap/backup

type Id = String
type Triple = (Id,FilePath,FilePath)

defaultDB = "~/Mail/id.db"

nomalizePath :: Id -> FilePath -> FilePath -> IO (Maybe Triple)
nomalizePath id db dir = do db' <- canonicalizePath =<< expandHome db
                            let dir' = noTrailingPathSeparator dir
                            return $ Just (id,db',dir')

expandHome :: FilePath -> IO FilePath
expandHome ('~':cs) = do home <- getHomeDirectory
                         return $ home ++ cs
expandHome dir      = return dir

noTrailingPathSeparator dir
    | hasTrailingPathSeparator dir = dropTrailingPathSeparator dir
    | otherwise                    = dir

----------------------------------------------------------------

main :: IO ()
main = do args <- getArgs
          let opts   = filter isOption args
              keys   = filter (not.isOption) args
              cmd    = parseOpts opts
          param <- parseKeys keys
          case exec cmd param of
            Just search -> search >>= printResults
            Nothing     -> help
    where
      isOption "" = False
      isOption (c:cs) = c == '-'
      exec (Just func) (Just ckey) = Just (dispatch func ckey)
      exec _           _           = Nothing
      printResults = mapM_ putStrLn

parseOpts opts
    | opts == []       = Just searchFamily
    | "-p" `elem` opts = Just searchPerant
    | "-c" `elem` opts = Just searchChild
    | otherwise        = Nothing

parseKeys :: [String] -> IO (Maybe Triple)
parseKeys []          = return Nothing
parseKeys [id]        = nomalizePath id defaultDB ""
parseKeys [id,db]     = nomalizePath id db ""
parseKeys [id,db,dir] = nomalizePath id db dir
parseKeys _           = return Nothing

----------------------------------------------------------------

type Message = [SqlValue]

getter n msg = case msg !! n of
                 SqlString x     -> x
                 SqlByteString x -> BS.unpack x
                 SqlNull         -> ""

myid  = getter 0
path  = getter 1
parid = getter 2
date  = getter 3
directory = takeDirectory.path

----------------------------------------------------------------

selectById :: Connection -> Id -> IO [Message]
selectById conn id = quickQuery' conn format params
    where
      format = "SELECT * FROM mew WHERE (mew.id = ?);"
      params = [toSql id]

selectByParid :: Connection -> [Id] -> IO [Message]
selectByParid conn ids = quickQuery' conn format params
    where
      fmt    = replicate (length ids) "mew.parid = ?"
      fmt'   = intersperse " OR " fmt
      fmt''  = concat fmt'
      format = "SELECT * FROM mew WHERE (" ++ fmt'' ++ ");"
      params = map toSql ids

----------------------------------------------------------------
-- to express failure, the empty list is used

chooseOne :: FilePath -> [Message] -> [Message]
chooseOne _ [] = [] -- failure
chooseOne "" (m:ms) = [m]
chooseOne dir msgs@(m:ms) = let sames = filter sameDir msgs
                            in if sames /= []
                               then [head sames]
                               else [m]
    where
      sameDir x = directory x == dir

----------------------------------------------------------------

type Search = Connection -> Id -> FilePath -> IO [FilePath]

dispatch :: Search -> Triple -> IO [FilePath]
dispatch func (id,db,dir) = do conn <- connectSqlite3 db
                               msgs <- func conn id dir
                               disconnect conn
                               return msgs

----------------------------------------------------------------

searchPerant :: Search
searchPerant conn id dir = do msgs <- selectById conn id 
                              return $ map path $ chooseOne dir msgs

----------------------------------------------------------------

searchChild :: Search
searchChild conn id dir = do msgs <- selectByParid conn [id]
                             return $ map path $ chooseOne dir msgs

----------------------------------------------------------------

searchFamily :: Search
searchFamily conn id dir = do rtid <- findRoot conn id
                              msgs <- findDescendants conn rtid dir
                              return $ map path $ qsort msgs

getParid :: Connection -> Id -> IO (Maybe Id)
getParid conn id = do msgs <- selectById conn id
                      return $ getPid msgs
    where
      getPid [] = Nothing
      getPid (e:es) = let pid = parid e
                      in if pid == ""
                         then Nothing
                         else Just pid

findRoot :: Connection -> Id -> IO Id
findRoot conn id = do parid <- getParid conn id
                      case parid of
                        Nothing  -> return id
                        Just pid -> findRoot conn pid

type Hash = Map Id Message

findDescendants :: Connection -> Id -> FilePath -> IO [Message]
findDescendants conn rtid dir = do roots <- selectById conn rtid
                                   let root = head $ chooseOne dir roots
                                       init = Map.insert rtid root Map.empty
                                   findChildren ([rtid],init)
    where
      findChildren :: ([Id],Hash) -> IO [Message]
      findChildren (ids,hash) =
          do msgs <- selectByParid conn ids
             if msgs == [] then return (Map.elems hash)
                           else findChildren $ pushChildren hash msgs []
      pushChildren :: Hash -> [Message] -> [Id] -> ([Id],Hash)
      pushChildren hash [] ids = (ids, hash)
      pushChildren hash (m:ms) ids
          | Map.member id hash = if directory m == dir
                                 then let hash'  = Map.delete id hash
                                          hash'' = Map.insert id m hash
                                      in pushChildren hash'' ms ids
                                 else pushChildren hash ms ids
          | otherwise         = pushChildren (Map.insert id m hash) ms (id:ids)
          where
            id = myid m

 
qsort :: [Message] -> [Message]
qsort [] = []
qsort (m:ms) =    qsort [x | x <- ms, date x  < p]
               ++ [m]
               ++ qsort [x | x <- ms, date x >= p]
    where
      p = date m
