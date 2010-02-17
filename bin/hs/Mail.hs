module Mail (parseMail) where

import Text.Parsec
import qualified Text.ParserCombinators.Parsec.Rfc2822NS as M (message)
import Text.ParserCombinators.Parsec.Rfc2822NS (Message(..), Field(..))
import System.Time
import Locale
import Msg

----------------------------------------------------------------

parseMail :: FilePath -> [Char] -> Maybe Msg
parseMail file cs = case parse M.message "" cs of
  Left  _       -> Nothing
  Right message -> makeMsg message file

makeMsg :: Message -> FilePath -> Maybe Msg
makeMsg message file = messageID message >>= \vmyid ->
  Just Msg { myid = vmyid
           , path = file
           , paid = messagePaID message
           , date = messageDate message
           }

----------------------------------------------------------------

messageID :: Message -> Maybe ID
messageID = messageField xMessageID getMessageID

{-
  (1) The In-Reply-To contains one ID, use it.
  (2) The References contains one or more IDs, use the last one.
  (3) The In-Reply-To contains two or more IDs, use the first one.
-}

messagePaID :: Message -> ID
messagePaID message
  | ilen == 1 = head is
  | rlen /= 0 = last rs
  | ilen /= 0 = head is
  | otherwise = ""
  where
    ilen = length is
    rlen = length rs
    is = maybe [] id (messageField xInReplyTo getInReplyTo message)
    rs = maybe [] id (messageField xReferences getReferences message)

messageDate :: Message -> String
messageDate message = maybe "9700101000000" (formatCalendarTime defaultTimeLocale "%Y%m%d%H%M%S") (messageField xdate getDate message)

messageField :: (Field -> Bool) -> (Field -> a) -> Message -> Maybe a
messageField p extract message
  | null rs   = Nothing
  | otherwise = Just (head rs)
  where
    getFields (Message fs _) = fs
    rs = map extract $ filter p $ getFields message

----------------------------------------------------------------

{-
 Design of hsemail-ns sucks. What boilerplates are!
-}

xMessageID :: Field -> Bool
xMessageID (MessageID _) = True
xMessageID _             = False

getMessageID :: Field -> String
getMessageID (MessageID x) = x
getMessageID _             = error "getMessageID"

xInReplyTo :: Field -> Bool
xInReplyTo (InReplyTo _) = True
xInReplyTo _             = False

getInReplyTo :: Field -> [String]
getInReplyTo (InReplyTo x) = x
getInReplyTo _             = error "getInReplyTo"

xReferences :: Field -> Bool
xReferences (References _) = True
xReferences _              = False

getReferences :: Field -> [String]
getReferences (References x) = x
getReferences _              = error "getReferences"

xdate :: Field -> Bool
xdate (Date _) = True
xdate _        = False

getDate :: Field -> CalendarTime
getDate (Date x) = x
getDate _        = error "getDate"
