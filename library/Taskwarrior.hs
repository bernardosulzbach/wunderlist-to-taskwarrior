module Taskwarrior where

import           Data.Char
import           Data.List
import           Data.Maybe
import           System.Exit
import           System.Process
import           Text.Read

-- Adds the provided description and returns the task UUID.
--
-- If the task could not be added, returns the empty string.
addTask :: String -> IO String
addTask description = do
  outputTuple <- readProcessWithExitCode "task" ["add", description] ""
  -- Could handle errors, but we can't really fix anything from here.
  let (code, output, _) = outputTuple
  if (code == ExitSuccess)
    then do
      -- We filter out punctuation because TaskWarrior puts a period at the end.
      let taskNumber = extractNumber (filter (not . isPunctuation) output)
      uuid <- getUUID taskNumber
      return uuid
    else return ""

-- Gets the UUID for the provided task number.
--
-- If the task does not exist, returns the empty string.
getUUID :: Int -> IO String
getUUID number = do
  outputTuple <- readProcessWithExitCode "task" [(show number), "info"] ""
  -- Could handle errors, but we can't really fix anything from here.
  let (code, output, _) = outputTuple
  if (code == ExitSuccess)
    then return (extractUUID output)
    else return ""

readInt :: String -> Maybe Int
readInt word = readMaybe word

-- Extract the first number of the provided string.
extractNumber :: String -> Int
extractNumber string = uuid
  where
    textWords = words string
    uuid = head (catMaybes (map readInt textWords))

-- Extract an UUID from the input text.
extractUUID :: String -> String
extractUUID string = uuid
  where
    textLines = lines string
    startsWithUUID line = isPrefixOf "UUID" line
    uuidLines = filter startsWithUUID textLines
    uuid = if (length uuidLines == 1)
             then last (words (head uuidLines))
             else ""
