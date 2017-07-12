import qualified Data.ByteString.Lazy as L
import           Prelude hiding (words)
import           Test.Hspec
import           Data.Ubjson
import           Data.Word
import           Data.Int

words :: Integral a => [a] -> [Word8]
words []      = [] :: [Word8]
words (x:xs)  = ((fromIntegral x) :: Word8) : words xs

main :: IO ()
main = hspec $ do
  describe "Data.Ubjson.encode" $ do
    it "should pack (True :: Bool) as Ubjson true" $ do
      (L.unpack $ encode True) `shouldBe` words [0x54]

    it "should pack (False :: Bool) as Ubjson false" $ do
      (L.unpack $ encode False) `shouldBe` words [0x46]

    it "should pack ('a' :: Char) as Ubjson char" $ do
      (L.unpack $ encode 'a') `shouldBe` ([0x43, 0x61] :: [Word8])

    it "should pack (163 :: Integer) as Ubjson uint8" $ do
      (L.unpack $ encode (163 :: Integer)) `shouldBe` words [0x55, 0xA3]

    it "should pack (-93 :: Integer) as Ubjson int8" $ do
      (L.unpack $ encode (-93 :: Integer)) `shouldBe` words [0x69, 0xA3]

    --it "should pack (64789 :: Integer) as Ubjson int16" $ do
    --      (L.unpack $ encode (64789 :: Integer)) `shouldBe` words [0x69, 0xA3]

    -- TODO more numerics here to be tested ...

    it "should pack (Nothing :: Maybe) as Ubjson null" $ do
      -- Note that it cannot be any Nothing, but rather a constrained one
      (L.unpack $ encode (Nothing :: Maybe Bool)) `shouldBe` [0x5A :: Word8]

    it "should pack Just 'A' :: Maybe" $ do
      -- Note that it cannot be any Nothing, but rather a constrained one
      (L.unpack $ encode (Just 'a')) `shouldBe` ([0x43, 0x61] :: [Word8])

