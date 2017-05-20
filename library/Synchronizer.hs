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

import           Control.Monad.IO.Class  (liftIO)
import qualified Data.Text
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Fetcher
import qualified Filesystem
import           Formatting
import           Formatting.Clock
import qualified Logger
import qualified Relation
import           System.Clock
import qualified Taskwarrior
import qualified Tokens
import qualified User
import qualified Wunderlist.Task

-- This is a possible bottleneck: a new SQLite connection is opened for each task.
--
-- In practice, however, this does not seem to hinder performance too much.
--
-- Returns True if the task was added by this call.
ensureAdded :: Wunderlist.Task.Task -> IO Bool
ensureAdded task = do
  databasePath <- Filesystem.databaseFilePath
  runSqlite (Data.Text.pack databasePath) $ do
    runMigration Relation.migrateAll
    relationsList <- selectList [Relation.RelationWunderlistId ==. taskId] []
    if (length relationsList == 0)
      then do
        uuid <- liftIO $ Taskwarrior.addTask taskTitle
        _ <- insert $ Relation.Relation taskId uuid
        return True
      else do
        return False

  where
    taskId = Wunderlist.Task.id task
    taskTitle = Wunderlist.Task.title task

logTaskInformation :: [Wunderlist.Task.Task] -> IO ()
logTaskInformation inboxTasks = Logger.log message
  where
    logString = "Inbox task count is " ++ (show (length inboxTasks)) ++ "."
    message = Data.Text.pack logString

logAddedInformation :: [Bool] -> IO ()
logAddedInformation added = Logger.log message
  where
    logString = "Added " ++ (show (length (filter id added))) ++ " new tasks."
    message = Data.Text.pack logString

synchronizeAll :: IO ()
synchronizeAll = Logger.withDefaultLogging $ do
  processStart <- getTime Monotonic
  eitherTokens <- Tokens.getDefaultTokens
  case eitherTokens of
    Left errorMessage -> putStrLn errorMessage
    Right tokens -> do
      let user = User.fromTokens tokens
      inbox <- Fetcher.fetchInbox user
      inboxTasks <- Fetcher.fetchTasks user inbox
      logTaskInformation inboxTasks
      updateStart <- getTime Monotonic
      added <- mapM ensureAdded inboxTasks
      updateEnd <- getTime Monotonic
      logAddedInformation added
      Logger.log (sformat ("Finished updating after " % timeSpecs % ".") updateStart updateEnd)
      processEnd <- getTime Monotonic
      Logger.log (sformat ("Finished running after " % timeSpecs % ".") processStart processEnd)
