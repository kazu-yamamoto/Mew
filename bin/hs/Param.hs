module Param where

type Regexp = String

defaultDB :: FilePath
defaultDB = "~/Mail/id.db"

defaultMailDir :: FilePath
defaultMailDir = "~/Mail"

defaultIgnoreRegex :: Regexp
defaultIgnoreRegex = "^casket" -- including casket_replica

defaultMessageRegex :: Regexp
defaultMessageRegex = "^[0-9]+(\\.mew)?$"

defaultDirModFile :: FilePath
defaultDirModFile = ".mew-mtime"

defaultTarget :: Maybe FilePath
defaultTarget = Nothing
