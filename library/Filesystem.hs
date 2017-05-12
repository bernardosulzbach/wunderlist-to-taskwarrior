module Filesystem where

import           System.Directory
import           System.FilePath

programDirectory :: String
programDirectory = ".wunderlist-to-taskwarrior"

databaseFilename :: String
databaseFilename = "db.sqlite3"

getFilePathForFilename :: String -> IO FilePath
getFilePathForFilename filename = do
  home <- getHomeDirectory
  return (joinPath [home, programDirectory, filename])

databaseFilePath :: IO FilePath
databaseFilePath = getFilePathForFilename databaseFilename
