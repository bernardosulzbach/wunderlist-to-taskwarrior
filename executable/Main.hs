{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as CLBS
import qualified Data.Text
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Fetcher
import qualified Filesystem
import qualified Relation
import qualified Tokens
import qualified User
import qualified Taskwarrior
import qualified Wunderlist.Task

printPretty :: ToJSON a => a -> IO ()
printPretty thing = CLBS.putStrLn $ encodePretty thing

-- This is a possible bottleneck: a new SQLite connection is opened for each task.
--
-- In practice, however, this does not seem to hinder performance too much.
ensureAdded :: Wunderlist.Task.Task -> IO ()
ensureAdded task = do
  databasePath <- Filesystem.databaseFilePath
  runSqlite (Data.Text.pack databasePath) $ do
    runMigration Relation.migrateAll
    relationsList <- selectList [Relation.RelationWunderlistId ==. taskId] []
    if (length relationsList == 0)
      then do
        uuid <- liftIO $ Taskwarrior.addTask taskTitle
        _ <- insert $ Relation.Relation taskId uuid
        return ()
      else do
        return ()

  where
    taskId = Wunderlist.Task.id task
    taskTitle = Wunderlist.Task.title task

main :: IO ()
main = do
  eitherTokens <- Tokens.getDefaultTokens
  case eitherTokens of
    Left errorMessage -> putStrLn errorMessage
    Right tokens -> do
      printPretty tokens
      let user = User.fromTokens tokens
      lists <- Fetcher.fetchLists user
      printPretty lists
      inbox <- Fetcher.fetchInbox user
      printPretty inbox
      inboxTasks <- Fetcher.fetchTasks user inbox
      printPretty inboxTasks
      mapM_ ensureAdded inboxTasks
      putStrLn "Done."
