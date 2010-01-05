----------------------------------------------------------------
--
-- smew.hs
--

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import System
import System.IO
import System.FilePath
import System.Directory
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.ByteString.Char8 as BS

----------------------------------------------------------------

helpMessage :: String
helpMessage = "[-p|-c] id [db [dir]]"

help :: IO ()
help = do
    prog <- getProgName
    hPutStrLn stderr $ prog ++ " " ++ helpMessage

----------------------------------------------------------------
-- smew '<20081014.105255.216914257.kazu@iij.ad.jp>' ~/Mail/id.db #imap/kazu@localhost#imap/backup

type Id = String
type Triple = (Id,FilePath,FilePath)

defaultDB :: String
defaultDB = "~/Mail/id.db"

nomalizePath :: Id -> FilePath -> FilePath -> IO (Maybe Triple)
nomalizePath mid db dir = do
    db' <- canonicalizePath =<< expandHome db
    let dir' = noTrailingPathSeparator dir
    return $ Just (mid,db',dir')

expandHome :: FilePath -> IO FilePath
expandHome ('~':cs) = do
    home <- getHomeDirectory
    return $ home ++ cs
expandHome dir      = return dir

noTrailingPathSeparator :: FilePath -> FilePath
noTrailingPathSeparator dir
    | hasTrailingPathSeparator dir = dropTrailingPathSeparator dir
    | otherwise                    = dir

----------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let opts = filter isOption args
        keys = filter (not.isOption) args
        cmd  = parseOpts opts
    param <- parseKeys keys
    maybe help (>>= printResults) (exec cmd param)
  where
    isOption "" = False
    isOption (c:_) = c == '-'
    exec (Just func) (Just ckey) = Just (dispatch func ckey)
    exec _           _           = Nothing
    printResults = mapM_ putStrLn

parseOpts :: [[Char]] -> Maybe (Connection -> Id -> FilePath -> IO [FilePath])
parseOpts opts
    | opts == []       = Just searchFamily
    | "-p" `elem` opts = Just searchPerant
    | "-c" `elem` opts = Just searchChild
    | otherwise        = Nothing

parseKeys :: [String] -> IO (Maybe Triple)
parseKeys []           = return Nothing
parseKeys [mid]        = nomalizePath mid defaultDB ""
parseKeys [mid,db]     = nomalizePath mid db ""
parseKeys [mid,db,dir] = nomalizePath mid db dir
parseKeys _            = return Nothing

----------------------------------------------------------------

type Message = [SqlValue]

getter :: Int -> [SqlValue] -> String
getter n msg = case msg !! n of
                 SqlString x     -> x
                 SqlByteString x -> BS.unpack x
                 SqlNull         -> ""
                 _               -> error "getter"

myid :: [SqlValue] -> String
myid  = getter 0
path :: [SqlValue] -> String
path  = getter 1
parid :: [SqlValue] -> String
parid = getter 2
date :: [SqlValue] -> String
date  = getter 3
directory :: [SqlValue] -> FilePath
directory = takeDirectory.path

----------------------------------------------------------------

selectById :: Connection -> Id -> IO [Message]
selectById conn mid = quickQuery' conn format params
  where
    format = "SELECT * FROM mew WHERE (mew.id = ?);"
    params = [toSql mid]

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
chooseOne "" (m:_) = [m]
chooseOne dir msgs@(m:_) =
    let sames = filter sameDir msgs
    in if sames /= []
       then [head sames]
       else [m]
  where
    sameDir x = directory x == dir

----------------------------------------------------------------

type Search = Connection -> Id -> FilePath -> IO [FilePath]

dispatch :: Search -> Triple -> IO [FilePath]
dispatch func (mid,db,dir) = do
    conn <- connectSqlite3 db
    msgs <- func conn mid dir
    disconnect conn
    return msgs

----------------------------------------------------------------

searchPerant :: Search
searchPerant conn mid dir = do
    msgs <- selectById conn mid 
    return $ map path $ chooseOne dir msgs

----------------------------------------------------------------

searchChild :: Search
searchChild conn mid dir = do
    msgs <- selectByParid conn [mid]
    return $ map path $ chooseOne dir msgs

----------------------------------------------------------------

searchFamily :: Search
searchFamily conn mid dir = do
    rtid <- findRoot conn mid
    msgs <- findDescendants conn rtid dir
    return $ map path $ sortBy (comparing date) msgs

getParid :: Connection -> Id -> IO (Maybe Id)
getParid conn mid = do
    msgs <- selectById conn mid
    return $ getPid msgs
  where
    getPid [] = Nothing
    getPid (e:_) = let pid = parid e
                   in if pid == ""
                      then Nothing
                      else Just pid

findRoot :: Connection -> Id -> IO Id
findRoot conn mid = getParid conn mid >>= maybe (return mid) (findRoot conn)

type Hash = Map Id Message

findDescendants :: Connection -> Id -> FilePath -> IO [Message]
findDescendants conn rtid dir = do
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
        | Map.member mid hash = if directory m == dir
                                then let hash'  = Map.delete mid hash
                                         hash'' = Map.insert mid m hash'
                                     in pushChildren hash'' ms ids
                                else pushChildren hash ms ids
        | otherwise         = pushChildren (Map.insert mid m hash) ms (mid:ids)
      where
        mid = myid m
