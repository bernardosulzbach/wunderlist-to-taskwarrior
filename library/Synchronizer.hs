{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Synchronizer where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.Text
import           Data.Time.LocalTime
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Fetcher
import qualified Filesystem
import           Formatting
import           Formatting.Clock
import           Formatting.Time
import qualified Logger
import qualified Relation
import           System.Clock
import qualified Taskwarrior
import qualified Tokens
import qualified User
import qualified Wunderlist.List
import qualified Wunderlist.Task

-- This is a possible bottleneck: a new SQLite connection is opened for each task.
--
-- In practice, however, this does not seem to hinder performance too much.
--
-- Returns True if the task was added by this call.
ensureAdded :: String -> Wunderlist.Task.Task -> IO Bool
ensureAdded project task = do
  databasePath <- Filesystem.databaseFilePath
  runSqlite (Data.Text.pack databasePath) $ do
    runMigration Relation.migrateAll
    relationsList <- selectList [Relation.RelationWunderlistId ==. taskId] []
    if (length relationsList == 0)
      then do
        uuid <- liftIO $ Taskwarrior.addTask project taskTitle
        _ <- insert $ Relation.Relation taskId uuid
        return True
      else do
        return False

  where
    taskId = Wunderlist.Task.id task
    taskTitle = Wunderlist.Task.title task

logAddedInformation :: Int -> IO ()
logAddedInformation added = Logger.log message
  where
    logString = "Added " ++ (show added) ++ " new tasks."
    message = Data.Text.pack logString

titleDoesNotStartWith :: String -> Wunderlist.List.List -> Bool
titleDoesNotStartWith prefix list = take (length prefix) title /= prefix
  where
    title = Wunderlist.List.title list

separateInbox :: [Wunderlist.List.List] -> (Wunderlist.List.List, [Wunderlist.List.List])
separateInbox lists = (head (filter isInbox lists), filter (not . isInbox) lists)
  where
    isInbox = Wunderlist.List.isInbox

-- Filters and extracts the Taskwarrior project name for a Wunderlist list.
prepareLists :: [Wunderlist.List.List] -> [(String, Wunderlist.List.List)]
prepareLists lists = ("", inbox) : pairedLists
  where
    (inbox, otherLists) = separateInbox lists
    goodLists = filter (titleDoesNotStartWith "!") otherLists
    pairedLists = zip (map Wunderlist.List.title goodLists) goodLists

retrieveTasks :: MVar () -> User.User -> (String, Wunderlist.List.List) -> IO (String, [Wunderlist.Task.Task])
retrieveTasks mvar user (project, list) = do
  tasks <- Fetcher.fetchTasks mvar user list
  return (project, tasks)

synchronizeList :: (String, [Wunderlist.Task.Task]) -> IO Int
synchronizeList (project, tasks) = do
  added <- mapM (ensureAdded project) tasks
  return $ length (filter id added)

synchronizeAll :: IO ()
synchronizeAll = do
  zonedTime <- getZonedTime
  processStart <- getTime Monotonic
  Logger.log (sformat ("Started running at " % dateDash % " " % hms % ".") zonedTime zonedTime)
  eitherTokens <- Tokens.getDefaultTokens
  case eitherTokens of
    Left errorMessage -> putStrLn errorMessage
    Right tokens -> do
      mvar <- newEmptyMVar
      let user = User.fromTokens tokens
      lists <- Fetcher.fetchLists mvar user
      let preparedLists = prepareLists lists
      -- Fetch each list and use it to update Taskwarrior.
      retrievedTasks <- mapConcurrently (retrieveTasks mvar user) preparedLists
      updateStart <- getTime Monotonic
      counters <- mapM synchronizeList retrievedTasks
      logAddedInformation (sum counters)
      updateEnd <- getTime Monotonic
      Logger.log (sformat ("Finished updating after " % timeSpecs % ".") updateStart updateEnd)
      processEnd <- getTime Monotonic
      Logger.log (sformat ("Finished running after " % timeSpecs % ".") processStart processEnd)
