{-# LANGUAGE OverloadedStrings #-}

module Fetcher where

import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map              as Map
import qualified Data.Text             as Text
import           Network.HTTP.Simple
import qualified User
import qualified Wunderlist.List
import qualified Wunderlist.Task

listsRequest :: Request
listsRequest = "GET http://a.wunderlist.com/api/v1/lists"

tasksRequest :: Request
tasksRequest = "GET http://a.wunderlist.com/api/v1/tasks"

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
  response <- httpJSON request
  return (getResponseBody response :: [Wunderlist.List.List])

fetchInbox :: User.User -> IO Wunderlist.List.List
fetchInbox user = do
  lists <- fetchLists user
  return $ head $ filter nameIsInbox lists

  where
    nameIsInbox list = (Wunderlist.List.title list) == "inbox"

fetchTasks :: User.User -> Wunderlist.List.List -> IO [Wunderlist.Task.Task]
fetchTasks user list = do
  let baseRequest = fillRequest user tasksRequest
  let listId = Just (C8.pack (show (Wunderlist.List.id list)))
  let request = setRequestQueryString [("list_id", listId)] baseRequest
  response <- httpJSON request
  return (getResponseBody response :: [Wunderlist.Task.Task])
