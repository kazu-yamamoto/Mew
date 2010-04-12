{-# LANGUAGE CPP #-}

#include "config.h"

module Param where

defaultDB :: String
defaultDB = "~/Mail/id.db"

defaultMailDir :: String
defaultMailDir = "~/Mail"

defaultIgnoreRegex :: String
defaultIgnoreRegex = "/casket$"

defaultMessageRegex :: String
#ifdef HAVE_WINDOWS_H
defaultMessageRegex = "\\\\[0-9]+(\\.mew)?$"
#else
defaultMessageRegex = "/[0-9]+(\\.mew)?$"
#endif

defaultDirModFile :: String
defaultDirModFile = ".mew-mtime"
