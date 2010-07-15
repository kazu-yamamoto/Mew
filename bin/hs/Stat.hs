{-# LANGUAGE CPP #-}

module Stat where

import Control.Applicative

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Win32.File
import System.Win32.Time
#else
import System.Posix.Files
#endif

isSymlink :: FilePath -> IO Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
isSymlink _ = return False
#else
isSymlink file = isSymbolicLink <$> getSymbolicLinkStatus file
#endif

getLinkCount :: FilePath -> IO (Maybe Int)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
getLinkCount _ = return Nothing
#else
getLinkCount file = Just . fromIntegral . linkCount <$> getFileStatus file
#endif

getStatusChangeTime :: FilePath -> IO Integer
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
getStatusChangeTime file = do
    fh <- createFile file gENERIC_READ fILE_SHARE_READ Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing
    (ctime,_,_) <- getFileTime fh
    closeHandle fh
    return . fileTimeToUnixTime $ ctime
  where
    -- http://support.microsoft.com/kb/167296/en-us
    -- 100 nano seconds since 1 Jan 1601
    -- MS: _FILETIME = {DWORD,DWORD} = {Word32,Word32}
    -- Haskell: FILETIME == DDWORD == Word64
    fileTimeToUnixTime (FILETIME x) = (fromIntegral x - 116444736000000000) `div` 10000000
#else
getStatusChangeTime file = realToInteger . statusChangeTime <$> getFileStatus file
  where
    -- EpochTime is not Integral, sigh
    realToInteger :: Real a => a -> Integer
    realToInteger x = let y = realToFrac x :: Double -- preventing warning
                      in round y
#endif
