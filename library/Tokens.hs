{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tokens where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics
import           System.Directory
import           System.FilePath

data Tokens = Tokens { clientId :: String, clientSecret :: String, accessToken :: String }
  deriving (Generic, Show)

instance FromJSON Tokens

instance ToJSON Tokens

getDefaultFilename :: IO FilePath
getDefaultFilename = do
  home <- getHomeDirectory
  let path = joinPath [home, ".tokens.json"]
  return path

getDefaultTokens :: IO (Either String Tokens)
getDefaultTokens = do
  filename <- getDefaultFilename
  defaultFile <- LBS.readFile filename
  let decoded = eitherDecode defaultFile
  return decoded
