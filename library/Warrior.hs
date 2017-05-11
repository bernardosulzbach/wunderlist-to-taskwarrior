module Warrior where

import           System.Exit
import           System.Process

getTaskCommand :: String -> String
getTaskCommand command = "task" ++ " " ++ command ++ " "

-- Adds the provided description and returns the UUID.
addTask :: String -> IO String
addTask description = do
  outputTuple <- readProcessWithExitCode (getTaskCommand "add") [description] ""
  let (code, output, errors) = outputTuple
  case code of
    ExitSuccess             -> return output
    ExitFailure failureCode -> return ""
