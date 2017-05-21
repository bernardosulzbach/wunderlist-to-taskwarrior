{-# LANGUAGE OverloadedStrings #-}

module Fetcher where

import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Lazy        as T
import           Formatting
import           Formatting.Clock
import qualified Logger
import           Network.HTTP.Simple
import           System.Clock
import qualified User
import qualified Wunderlist.List
import qualified Wunderlist.Task

listsRequest :: Request
listsRequest = "GET http://a.wunderlist.com/api/v1/lists"

tasksRequest :: Request
tasksRequest = "GET http://a.wunderlist.com/api/v1/tasks"

-- Works like httpJSON, but logs request duration from start to finish.
httpJSONLogged :: FromJSON a => Request -> String -> IO (Response a)
httpJSONLogged request description = do
  start <- getTime Monotonic
  let info = T.pack description
  Logger.log (sformat ("Started " % text % ".") info)
  response <- httpJSON request
  end <- getTime Monotonic
  Logger.log (sformat ("Finished " % text % " after " % timeSpecs % ".") info start end)
  return response

fillRequest :: User.User -> Request -> Request
fillRequest user request = withEverything
  where
    clientId = [C8.pack (User.clientId user)]
    withClientId = setRequestHeader "X-Client-ID" clientId request
    accessToken = [C8.pack (User.accessToken user)]
    withEverything = setRequestHeader "X-Access-Token" accessToken $ withClientId

fetchLists :: User.User -> IO [Wunderlist.List.List]
fetchLists user = do
  let request = fillRequest user listsRequest
  response <- httpJSONLogged request "fetching all lists"
  return (getResponseBody response :: [Wunderlist.List.List])

fetchTasks :: User.User -> Wunderlist.List.List -> IO [Wunderlist.Task.Task]
fetchTasks user list = do
  let baseRequest = fillRequest user tasksRequest
  let listId = Just (C8.pack (show (Wunderlist.List.id list)))
  let request = setRequestQueryString [("list_id", listId)] baseRequest
  response <- httpJSONLogged request "fetching tasks"
  return (getResponseBody response :: [Wunderlist.Task.Task])
