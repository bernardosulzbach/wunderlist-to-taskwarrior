module User where

data User = User { clientId :: String, accessToken :: String }
  deriving (Show)
