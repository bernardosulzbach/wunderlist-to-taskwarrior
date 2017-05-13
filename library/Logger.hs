{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logger where

import           Control.Logging
import qualified Data.Text
import qualified Filesystem

formatString :: String
formatString = "%Y-%m-%d %H:%M:%S.%q"

withDefaultLogging :: IO a -> IO a
withDefaultLogging a = do
  logFilePath <- Filesystem.logFilePath
  withLogging <- withFileLogging logFilePath $ do
                   setLogTimeFormat formatString
                   result <- a
                   return result
  return withLogging

log :: Data.Text.Text -> IO ()
log a = Control.Logging.log a
