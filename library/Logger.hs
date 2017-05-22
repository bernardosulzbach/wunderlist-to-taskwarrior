{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Logger where

import           Control.Concurrent.MVar
import           Control.Monad.Logger
import qualified Data.Text               as T
import qualified Filesystem

formatString :: String
formatString = "%Y-%m-%d %H:%M:%S.%q"

log :: T.Text -> IO ()
log a = do
  logFilePath <- Filesystem.logFilePath
  runFileLoggingT logFilePath ($(logInfo) a)

logS :: MVar () -> T.Text -> IO ()
logS m a = do
  putMVar m ()
  Logger.log a
  takeMVar m
