{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.ByteString           as B
import           Logger
import           System.Posix
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck     as QC

main :: IO ()
main = defaultMain $ testGroup "trimByteString" trimByteStringProperties

newtype ACOff = ACOff COff
  deriving (Show)

instance Arbitrary ACOff where
  arbitrary = ACOff `liftM` (liftM (fromIntegral :: Int -> COff) (choose (0, 127)))

unACOff :: ACOff -> COff
unACOff (ACOff off) = off

trimByteStringProperties :: [TestTree]
trimByteStringProperties =
  [ QC.testProperty "upper limit is respected" trimByteStringShouldRespectTheUpperLimit
  , QC.testProperty "no line is corrupted" trimByteStringShouldNotCorruptLines
  ]

trimByteStringShouldRespectTheUpperLimit :: B.ByteString -> ACOff -> Bool
trimByteStringShouldRespectTheUpperLimit b s = trimmedSize <= asCOff
  where
    asCOff = unACOff s
    trimmedSize = fromIntegral (B.length (Logger.trimByteString b asCOff)) :: COff

trimByteStringShouldNotCorruptLines :: B.ByteString -> ACOff -> Bool
trimByteStringShouldNotCorruptLines b s = trimmedLines == expectedLines
  where
    asCOff = unACOff s
    newline = head (B.unpack "\n")
    sourceLines = B.split newline b
    trimmedLines = B.split newline (Logger.trimByteString b asCOff)
    expectedLines = drop (length sourceLines - length trimmedLines) sourceLines
