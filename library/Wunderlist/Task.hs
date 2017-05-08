{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Wunderlist.Task where

import           Data.Aeson
import           GHC.Generics

data Task = Task { id :: Int, title :: String }
  deriving (Generic, Show)

instance FromJSON Task

instance ToJSON Task
