{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Logger (Logger.log, Logger.logS) where

import           Control.Concurrent.MVar
import           Control.Monad.Logger
import qualified Data.ByteString         as B
import qualified Data.Text               as T
import qualified Filesystem
import           System.Posix

mebibyte :: FileOffset
mebibyte = (2 :: FileOffset) ^ (20 :: FileOffset)

maximumLogFileSize :: FileOffset
maximumLogFileSize = 8 * mebibyte

trimmedLogFileSize :: FileOffset
trimmedLogFileSize = 4 * mebibyte

getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
  stat <- getFileStatus path
  return (fileSize stat)

effectivelyTrim :: FilePath -> FileOffset -> IO ()
effectivelyTrim path size = do
  bytes <- B.readFile path
  let intSize = fromIntegral size :: Int
  -- The minimum number of bytes we have to drop.
  let minDrop = (B.length bytes) - intSize
  -- Drop the minimum minus one.
  let trimmed = B.drop (minDrop - 1) bytes
  -- Drop until you dropped a '\n'.
  let newline = head (B.unpack "\n")
  let cleared = B.tail (B.dropWhile (/= newline) trimmed)
  B.writeFile path cleared

-- Trims the log file if needed.
trim :: FilePath -> FileOffset -> FileOffset -> IO ()
trim path maximumSize trimmed = do
  logFileSize <- getFileSize path
  if logFileSize > maximumSize
    then effectivelyTrim path trimmed
    else return ()

log :: T.Text -> IO ()
log a = do
  logFilePath <- Filesystem.logFilePath
  runFileLoggingT logFilePath ($(logInfo) a)
  trim logFilePath maximumLogFileSize trimmedLogFileSize

logS :: MVar () -> T.Text -> IO ()
logS m a = do
  putMVar m ()
  Logger.log a
  takeMVar m
