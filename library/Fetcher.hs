{-# LANGUAGE OverloadedStrings #-}

module Fetcher where

import qualified Data.ByteString.Char8 as C8
import           Network.HTTP.Simple
import qualified User
import qualified Wunderlist

baseFetchListsRequest :: Request
baseFetchListsRequest = "GET http://a.wunderlist.com/api/v1/lists"

fetchLists :: User.User -> IO ()
fetchLists user = do
  let request = setRequestHeader "X-Access-Token" [C8.pack $ User.accessToken user] $ setRequestHeader
                                                                                        "X-Client-ID"
                                                                                        [ C8.pack $ User.clientId
                                                                                                      user
                                                                                        ] $ baseFetchListsRequest
  response <- httpJSON request
  print $ (getResponseBody response :: [Wunderlist.List])
