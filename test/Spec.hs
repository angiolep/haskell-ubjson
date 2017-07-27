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

    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Noop
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode Noop as Ubjson noop" $ do
      let actual = L.unpack $ encode Noop
      actual `shouldBe` expected [0x4E]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Null
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode Null as Ubjson null" $ do
      let actual = L.unpack $ encode Null
      actual `shouldBe` expected [0x5A]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Bool
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode (True :: Bool) as Ubjson true" $ do
      let actual = L.unpack $ encode True
      actual `shouldBe` expected [0x54]

    it "should encode (False :: Bool) as Ubjson false" $ do
      let actual = L.unpack $ encode False
      actual `shouldBe` expected [0x46]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Char
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode ('a' :: Char) as Ubjson char" $ do
      let actual = L.unpack $ encode 'a'
      actual `shouldBe`  expected [0x43, 0x61]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Integers
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode (163 :: Int) as Ubjson uint8" $ do
      let actual = L.unpack $ encode (163 :: Int)
      actual `shouldBe` expected [0x55, 0xA3]

    it "should encode (-93 :: Int) as Ubjson int8" $ do
      let actual = L.unpack $ encode (-93 :: Int)
      actual `shouldBe` expected [0x69, 0xA3]

    it "should encode (256 :: Int) as Ubjson int16" $ do
      let actual = L.unpack $ encode (256 :: Int)
      actual `shouldBe` expected [0x49, 0x01, 0x00]

    it "should encode (32767 :: Int) as Ubjson int16" $ do
      let actual = L.unpack $ encode (32767 :: Int)
      actual `shouldBe` expected [0x49, 0x7F, 0xFF]

    it "should encode (32768 :: Int) as Ubjson int32" $ do
      let actual = L.unpack $ encode (32768 :: Int)
      actual `shouldBe` expected [0x6C, 0x00, 0x00, 0x80, 0x00]

    it "should encode (2147483647 :: Int) as Ubjson int32" $ do
      let actual = L.unpack $ encode (2147483647 :: Int)
      actual `shouldBe` expected [0x6C, 0x7F, 0xFF, 0xFF, 0xFF]

    it "should encode (2147483648 :: Int) as Ubjson int64" $ do
      let actual = L.unpack $ encode (2147483648 :: Int)
      actual `shouldBe` expected [ 0x4C, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00 ]

--    it "should encode (9223372036854775807 :: Int) as Ubjson int64" $ do
--      let actual = (L.unpack $ encode (9223372036854775807 :: Int))
--      actual `shouldBe` expected [ 0x4C, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00 ]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Strings
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode (\"hello\" :: Text) as Ubjson string" $ do
      let actual = L.unpack $ encode (T.pack "hello")
      --                             S     U     5     h     e     l     l     0
      actual `shouldBe` expected [0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F]


    it "should encode (\"привет\" :: Text) as Ubjson string" $ do
      let actual = L.unpack $ encode (T.pack "привет")
      --                             S     U    12           п           р           и           в           е           т
      actual `shouldBe` expected [0x53, 0x55, 0x0C, 0xD0, 0xBF, 0xD1, 0x80, 0xD0, 0xB8, 0xD0, 0xB2, 0xD0, 0xB5, 0xD1, 0x82]


    it "should encode (\"مرحبا\" :: Text) as Ubjson string" $ do
      let actual = L.unpack $ encode (T.pack "مرحبا")
      --                             S     U    10           ا            ب           ح           ر           م
      actual `shouldBe` expected [0x53, 0x55, 0x0A, 0xD9, 0x85, 0xD8, 0xB1, 0xD8, 0xAD, 0xD8, 0xA8, 0xD8, 0xA7]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Maybe, Nothing and Just
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode (Nothing :: Maybe) as Ubjson null" $ do
      let actual = L.unpack $ encode (Nothing :: Maybe Bool)
      actual `shouldBe` expected [0x5A]


    it "should encode (Just 'c' :: Maybe) as Ubjson char" $ do
      let actual = L.unpack $ encode (Just 'c')
      actual `shouldBe` expected [0x43, 0x63]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Lists of homogeneous chars, integrals, booleans, etc.
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode ['a', 'b', 'c'] as Ubjson array" $ do
      let actual = L.unpack $ encode ['a','b','c']
      actual `shouldBe` expected [0x5B, 0x43, 0x61, 0x43, 0x62, 0x43, 0x63, 0x5D]

    it "should encode ([163, -93] :: Int) as Ubjson array" $ do
      let actual = L.unpack $ encode ([163, -93] :: [Int])
      actual `shouldBe` expected [0x5B, 0x55, 0xA3, 0x69, 0xA3, 0x5D]

    it "should encode [True, False, True, True] as Ubjson array" $ do
      let actual = L.unpack $ encode [True, False, True, True]
      actual `shouldBe` expected [0x5B, 0x54, 0x46, 0x54, 0x54, 0x5D]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Lists of heterogeneous values
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode heterogeneous list as Ubjson array" $ do
      let hlist = [Ubj Noop, Ubj Null, Ubj True, Ubj 'a', Ubj (163 :: Int), Ubj $ T.pack "hello"]
      let actual = L.unpack $ encode hlist
      actual `shouldBe` expected [0x5B, 0x4E, 0x5A, 0x54, 0x43, 0x61, 0x55, 0xA3, 0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x5D]

