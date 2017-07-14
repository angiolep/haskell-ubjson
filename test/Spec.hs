
import qualified Data.Text as T
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

    it "should pack ('c' :: Char) as Ubjson char" $ do
      (L.unpack $ encode 'c') `shouldBe`  words [0x43, 0x63]

    it "should pack (163 :: Int) as Ubjson uint8" $ do
      (L.unpack $ encode (163 :: Int)) `shouldBe` words [0x55, 0xA3]

    it "should pack (-93 :: Int) as Ubjson int8" $ do
      (L.unpack $ encode (-93 :: Int)) `shouldBe` words [0x69, 0xA3]

    --it "should pack (64789 :: Int) as Ubjson int16" $ do
    --      (L.unpack $ encode (64789 :: Int)) `shouldBe` words [0x69, 0xA3]

    -- TODO more numerics here to be tested ...
    -- decimal : 5
    -- binary  : 0000 0101
    -- hex     : 0x05

    it "should pack (\"hello\" :: Text) as Ubjson string" $ do
      -- Note that it cannot be any Nothing, but rather a constrained one
      (L.unpack $ encode (T.pack "hello")) `shouldBe` words [0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F]

    it "should pack (Nothing :: Maybe) as Ubjson null" $ do
      -- Note that it cannot be any Nothing, but rather a constrained one
      (L.unpack $ encode (Nothing :: Maybe Bool)) `shouldBe` words [0x5A]

    it "should pack Just 'c' :: Maybe as Ubjson char" $ do
      -- Note that it cannot be any Nothing, but rather a constrained one
      (L.unpack $ encode (Just 'c')) `shouldBe` words [0x43, 0x63]

