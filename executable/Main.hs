{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as CLBS
import qualified Fetcher
import qualified Tokens
import qualified User
import qualified Warrior

printPretty :: ToJSON a => a -> IO ()
printPretty thing = CLBS.putStrLn $ encodePretty thing

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
      result <- Warrior.addTask "Testing the addTask function"
      putStrLn result
