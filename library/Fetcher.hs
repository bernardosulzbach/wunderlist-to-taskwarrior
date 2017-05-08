{-# LANGUAGE OverloadedStrings #-}

module Fetcher where

import           Data.Aeson
import           Data.ByteString.Lazy as LBS
import           Network.HTTP.Client
import           User

fetchLists :: User -> [String]
fetchLists user = []

buildRequest :: String -> RequestBody -> IO Request
buildRequest url body = do
  nakedRequest <- parseRequest url
  return (nakedRequest { method = "POST", requestBody = body })

send :: RequestBody -> IO LBS.ByteString
send s = do
  request <- buildRequest "http://httpbin.org/post" s
  manager <- newManager defaultManagerSettings
  response <- httpLbs request manager
  return (responseBody response)
