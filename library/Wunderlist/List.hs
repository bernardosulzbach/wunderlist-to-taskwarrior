{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Wunderlist.List where

import           Data.Aeson
import           GHC.Generics

data List = List { id :: Int, title :: String }
  deriving (Generic, Show)

isInbox :: List -> Bool
isInbox a = title a == "inbox"

instance FromJSON List

instance ToJSON List
