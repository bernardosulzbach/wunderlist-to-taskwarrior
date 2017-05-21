{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Logger where

import           Control.Monad.Logger
import qualified Data.Text
import qualified Filesystem

formatString :: String
formatString = "%Y-%m-%d %H:%M:%S.%q"

log :: Data.Text.Text -> IO ()
log a = do
  logFilePath <- Filesystem.logFilePath
  runFileLoggingT logFilePath ($(logInfo) a)
