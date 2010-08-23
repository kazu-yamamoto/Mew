module Param where

defaultDB :: String
defaultDB = "~/Mail/id.db"

defaultMailDir :: String
defaultMailDir = "~/Mail"

defaultIgnoreRegex :: String
defaultIgnoreRegex = "^casket" -- including casket_replica

defaultMessageRegex :: String
defaultMessageRegex = "^[0-9]+(\\.mew)?$"

defaultDirModFile :: String
defaultDirModFile = ".mew-mtime"
