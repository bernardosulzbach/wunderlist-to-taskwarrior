{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Wunderlist where

import           Data.Aeson
import           GHC.Generics

data List = List { id :: Int, title :: String }
  deriving (Generic, Show)

instance FromJSON List

instance ToJSON List
