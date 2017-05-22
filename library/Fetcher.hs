{-# LANGUAGE OverloadedStrings #-}

module Fetcher where

import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.ByteString.Char8   as C
import qualified Data.Text.Lazy          as L
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
httpJSONLogged :: FromJSON a => MVar () -> Request -> L.Text -> IO (Response a)
httpJSONLogged mvar request info = do
  start <- getTime Monotonic
  Logger.logS mvar (sformat ("Started " % text % ".") info)
  response <- httpJSON request
  end <- getTime Monotonic
  Logger.logS mvar (sformat ("Finished " % text % " after " % timeSpecs % ".") info start end)
  return response

fillRequest :: User.User -> Request -> Request
fillRequest user request = withEverything
  where
    clientId = [C.pack (User.clientId user)]
    withClientId = setRequestHeader "X-Client-ID" clientId request
    accessToken = [C.pack (User.accessToken user)]
    withEverything = setRequestHeader "X-Access-Token" accessToken $ withClientId

fetchLists :: MVar () -> User.User -> IO [Wunderlist.List.List]
fetchLists mvar user = do
  let request = fillRequest user listsRequest
  response <- httpJSONLogged mvar request "fetching all lists"
  return (getResponseBody response :: [Wunderlist.List.List])

fetchTasks :: MVar () -> User.User -> Wunderlist.List.List -> IO [Wunderlist.Task.Task]
fetchTasks mvar user list = do
  let baseRequest = fillRequest user tasksRequest
  let listId = Just (C.pack (show (Wunderlist.List.id list)))
  let listTitle = L.pack (Wunderlist.List.title list)
  let request = setRequestQueryString [("list_id", listId)] baseRequest
  let logMessage = format ("fetching tasks for \"" % text % "\"") listTitle
  response <- httpJSONLogged mvar request logMessage
  return (getResponseBody response :: [Wunderlist.Task.Task])
