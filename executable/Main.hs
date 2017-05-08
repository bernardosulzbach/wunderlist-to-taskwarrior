{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as LBS
import qualified Fetcher
import qualified Tokens
import qualified User

printPretty thing = LBS.putStrLn $ encodePretty thing

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
