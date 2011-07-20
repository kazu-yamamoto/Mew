{-# LANGUAGE CPP #-}

module Mail (fileMsg) where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Time
import Locale
import Msg
import System.IO
import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------

type Key = String
newtype Value = Value { foldedLines :: [String] } deriving Show
type Header = [(Key,Value)]

getValue :: Key -> [(Key,Value)] -> Maybe String
getValue key fs = concat . foldedLines <$> lookup key fs

----------------------------------------------------------------

fileMsg :: FilePath -> String -> IO (Maybe Msg)
fileMsg file folder = makeMsg folder . header <$> readFileU8 file
  where
    readFileU8 fl = do
        h <- openFile fl ReadMode
#if __GLASGOW_HASKELL__ >= 611
        hSetEncoding h latin1
#endif
        hGetContents h

header :: String -> Header
header = unfold . takeWhile (/= "") . lines

unfold :: [String] -> Header
unfold [] = []
unfold (l:ls) = unfold' $ break (== ':') l
  where
    unfold' (_,[])   = unfold ls'
    unfold' (k,_:v') = (key, Value (v:vs)) : unfold ls'
      where
        key = map toLower k
        v   = dropWhile isSpace v'
    vs  = takeWhile (isSpace . head) ls
    ls' = dropWhile (isSpace . head) ls

makeMsg :: FilePath -> Header -> Maybe Msg
makeMsg folder hdr = messageID hdr >>= \vmyid ->
  Just Msg { 
      myid = vmyid
    , path = folder
    , paid = messagePaID hdr
    , date = messageDate hdr
    }

----------------------------------------------------------------

messageID :: Header -> Maybe ID
messageID hdr = getValue "message-id" hdr >>= parseMaybe msgid

{-
  (1) The In-Reply-To contains one ID, use it.
  (2) The References contains one or more IDs, use the last one.
  (3) The In-Reply-To contains two or more IDs, use the first one.
-}

messagePaID :: Header -> ID
messagePaID hdr
  | ilen == 1 = head is
  | rlen /= 0 = last rs
  | ilen /= 0 = head is
  | otherwise = ""
  where
    ilen = length is
    rlen = length rs
    is = fromMaybe [] inReplyTo
    rs = fromMaybe [] references
    inReplyTo  = getValue "in-reply-to" hdr >>= parseMaybe msgids
    references = getValue "references"  hdr >>= parseMaybe msgids

messageDate :: Header -> String
messageDate hdr = maybe "19700101000000" toStr (getValue "date" hdr >>= parseDate)
  where
    toStr :: UTCTime -> String
    toStr  = formatTime defaultTimeLocale "%Y%m%d%H%M%S"

parseDate :: String -> Maybe UTCTime
parseDate cs = parseTime defaultTimeLocale "%a, %e %b %Y %T %z" xs
  where
    (xs,_) = break (=='(') cs

----------------------------------------------------------------

parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p cs = either (const Nothing) Just (parse p "" cs)

----------------------------------------------------------------

msgid :: Parser String
msgid = do
    char '<'
    left  <- many1 (oneOf dotAtom)
    char '@'
    right <- many1 (oneOf dotAtom)
    char '>'
    spaces
    return $ "<" ++ left ++ "@" ++ right ++ ">"
  where
    dotAtom = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
           ++ "!#$%&'*+-/=?^_`{|}~."

msgids :: Parser [String]
msgids = many1 msgid
