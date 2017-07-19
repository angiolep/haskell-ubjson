
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import           Prelude
import           Test.Hspec
import           Data.Ubjson
import           Data.Word
import           Data.Int

expected :: Integral a => [a] -> [Word8]
expected []      = [] :: [Word8]
expected (x:xs)  = ((fromIntegral x) :: Word8) : expected xs

main :: IO ()
main = hspec $ do
  describe "Data.Ubjson.encode" $ do

    it "should encode (True :: Bool) as Ubjson true" $ do
      let actual = (L.unpack $ encode True)
      actual `shouldBe` expected [0x54]


    it "should encode (False :: Bool) as Ubjson false" $ do
      let actual = (L.unpack $ encode False)
      actual `shouldBe` expected [0x46]


    it "should encode ('c' :: Char) as Ubjson char" $ do
      let actual = (L.unpack $ encode 'c')
      actual `shouldBe`  expected [0x43, 0x63]


    it "should encode (163 :: Int) as Ubjson uint8" $ do
      let actual = (L.unpack $ encode (163 :: Int))
      actual `shouldBe` expected [0x55, 0xA3]


    it "should encode (-93 :: Int) as Ubjson int8" $ do
      let actual = (L.unpack $ encode (-93 :: Int))
      actual `shouldBe` expected [0x69, 0xA3]

    --it "should encode (64789 :: Int) as Ubjson int16" $ do
    --      (L.unpack $ encode (64789 :: Int)) `shouldBe` expected [0x69, 0xA3]

    -- TODO more numerics here to be tested ...
    -- decimal : 5
    -- binary  : 0000 0101
    -- hex     : 0x05

    it "should encode (\"hello\" :: Text) as Ubjson string" $ do
      let actual = (L.unpack $ encode (T.pack "hello"))
      --                             S     U     5     h     e     l     l     0
      actual `shouldBe` expected [0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F]


    it "should encode (\"привет\" :: Text) as Ubjson string" $ do
      let actual = (L.unpack $ encode (T.pack "привет"))
      --                             S     U    12           п           р           и           в           е           т
      actual `shouldBe` expected [0x53, 0x55, 0x0C, 0xD0, 0xBF, 0xD1, 0x80, 0xD0, 0xB8, 0xD0, 0xB2, 0xD0, 0xB5, 0xD1, 0x82]


    it "should encode (\"مرحبا\" :: Text) as Ubjson string" $ do
      let actual = (L.unpack $ encode (T.pack "مرحبا"))
      --                             S     U    10           ا            ب           ح           ر           م
      actual `shouldBe` expected [0x53, 0x55, 0x0A, 0xD9, 0x85, 0xD8, 0xB1, 0xD8, 0xAD, 0xD8, 0xA8, 0xD8, 0xA7]


    it "should encode (Nothing :: Maybe) as Ubjson null" $ do
      let actual = (L.unpack $ encode (Nothing :: Maybe Bool))
      actual `shouldBe` expected [0x5A]


    it "should encode Just 'c' :: Maybe as Ubjson char" $ do
      let actual = (L.unpack $ encode (Just 'c'))
      actual `shouldBe` expected [0x43, 0x63]


    it "should encode ['a', 'b', 'c'] as Ubjson array" $ do
      let actual = (L.unpack $ encode ['a','b','c'])
      actual `shouldBe` expected [0x5B, 0x43, 0x61, 0x43, 0x62, 0x43, 0x63, 0x5D]


    it "should encode [163, -93] as Ubjson array" $ do
      let actual = (L.unpack $ encode ([163, -93] :: [Int]))
      actual `shouldBe` expected [0x5B, 0x55, 0xA3, 0x69, 0xA3, 0x5D]
