{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tokens where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics

data Tokens = Tokens { clientId :: String, clientSecret :: String, accessToken :: String }
  deriving (Generic, Show)

instance FromJSON Tokens

instance ToJSON Tokens

getDefaultFilename :: FilePath
getDefaultFilename = "tokens.json"

getDefaultTokens :: IO (Either String Tokens)
getDefaultTokens = do
  defaultFile <- LBS.readFile getDefaultFilename
  let decoded = eitherDecode defaultFile
  return decoded
