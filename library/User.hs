module User where

import qualified Tokens

data User = User { clientId :: String, accessToken :: String }
  deriving (Show)

fromTokens :: Tokens.Tokens -> User
fromTokens tokens = User (Tokens.clientId tokens) (Tokens.accessToken tokens)
