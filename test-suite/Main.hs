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
  arbitrary = ACOff `liftM` (liftM (fromIntegral :: Int -> COff) (choose (0, 100)))

unACOff :: ACOff -> COff
unACOff (ACOff off) = off

trimByteStringProperties :: [TestTree]
trimByteStringProperties =
  [QC.testProperty "upper limit is respected" trimByteStringShouldRespectTheUpperLimit]

trimByteStringShouldRespectTheUpperLimit :: B.ByteString -> ACOff -> Bool
trimByteStringShouldRespectTheUpperLimit b s = trimmedSize <= asCOff
  where
    asCOff = unACOff s
    trimmedSize = fromIntegral (B.length (Logger.trimByteString b asCOff)) :: COff
