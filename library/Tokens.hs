{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tokens where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Filesystem
import           GHC.Generics

data Tokens = Tokens { clientId :: String, clientSecret :: String, accessToken :: String }
  deriving (Generic, Show)

instance FromJSON Tokens

instance ToJSON Tokens

tokensFilename :: String
tokensFilename = "tokens.json"

getTokensFilePath :: IO FilePath
getTokensFilePath = Filesystem.getFilePathForFilename tokensFilename

getDefaultTokens :: IO (Either String Tokens)
getDefaultTokens = do
  path <- getTokensFilePath
  file <- LBS.readFile path
  return (eitherDecode file)
