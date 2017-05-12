module UI where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8

printPretty :: ToJSON a => a -> IO ()
printPretty thing = Data.ByteString.Lazy.Char8.putStrLn $ encodePretty thing
