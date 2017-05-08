{-# LANGUAGE OverloadedStrings #-}

import qualified Fetcher
import qualified Tokens
import qualified User

main :: IO ()
main = do
  eitherTokens <- Tokens.getDefaultTokens
  case eitherTokens of
    Left errorMessage -> putStrLn errorMessage
    Right tokens -> do
      print tokens
      let user = User.fromTokens tokens
      lists <- Fetcher.fetchLists user
      print lists
      inbox <- Fetcher.fetchInbox user
      print inbox
      inboxTasks <- Fetcher.fetchTasks user inbox
      print inboxTasks
