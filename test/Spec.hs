-- http://dmitry-ra.github.io/ubjson-test-suite/blocks-parser.html

import qualified Data.Text            as T
import qualified Data.Map             as M
import qualified Data.ByteString.Lazy as B
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
    -- noop
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode Noop as Ubjson noop" $ do
      let actual = B.unpack $ encode Noop
      actual `shouldBe` expected [0x4E]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- null
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode (Nothing :: Maybe a) as Ubjson null" $ do
      let actual = B.unpack $ encode (Nothing :: Maybe String)
      actual `shouldBe` expected [0x5A]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- bool
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode (True :: Bool) as Ubjson true" $ do
      let actual = B.unpack $ encode True
      actual `shouldBe` expected [0x54]

    it "should encode (False :: Bool) as Ubjson false" $ do
      let actual = B.unpack $ encode False
      actual `shouldBe` expected [0x46]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- char
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode ('h' :: Char) as Ubjson char" $ do
      let actual = B.unpack $ encode 'h'
      actual `shouldBe`  expected [0x43, 0x68]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- numbers
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode (0 :: Int) as Ubjson uint8" $ do
      let actual = B.unpack $ encode (0 :: Int)
      actual `shouldBe` expected [0x55, 0x00]

    it "should encode (163 :: Int) as Ubjson uint8" $ do
      let actual = B.unpack $ encode (163 :: Int)
      actual `shouldBe` expected [0x55, 0xA3]

    it "should encode (255 :: Int) as Ubjson uint8" $ do
      let actual = B.unpack $ encode (255 :: Int)
      actual `shouldBe` expected [0x55, 0xFF]


    -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

    it "should encode (-1 :: Int) as Ubjson int8" $ do
      let actual = B.unpack $ encode (-1 :: Int)
      actual `shouldBe` expected [0x69, 0xFF]

    it "should encode (-93 :: Int) as Ubjson int8" $ do
      let actual = B.unpack $ encode (-93 :: Int)
      actual `shouldBe` expected [0x69, 0xA3]

    it "should encode (256 :: Int) as Ubjson int16" $ do
      let actual = B.unpack $ encode (256 :: Int)
      actual `shouldBe` expected [0x49, 0x01, 0x00]


    -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

    it "should encode (-32768 :: Int) as Ubjson int16" $ do
      let actual = B.unpack $ encode (-32768 :: Int)
      actual `shouldBe` expected [0x49, 0x80, 0x00]

    it "should encode (-129 :: Int) as Ubjson int16" $ do
      let actual = B.unpack $ encode (-129 :: Int)
      actual `shouldBe` expected [0x49, 0xFF, 0x7F]

    it "should encode (256 :: Int) as Ubjson int16" $ do
      let actual = B.unpack $ encode (256 :: Int)
      actual `shouldBe` expected [0x49, 0x01, 0x00]

    it "should encode (32767 :: Int) as Ubjson int16" $ do
      let actual = B.unpack $ encode (32767 :: Int)
      actual `shouldBe` expected [0x49, 0x7F, 0xFF]


    -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

    it "should encode (32768 :: Int) as Ubjson int32" $ do
      let actual = B.unpack $ encode (32768 :: Int)
      actual `shouldBe` expected [0x6C, 0x00, 0x00, 0x80, 0x00]

    it "should encode (2147483647 :: Int) as Ubjson int32" $ do
      let actual = B.unpack $ encode (2147483647 :: Int)
      actual `shouldBe` expected [0x6C, 0x7F, 0xFF, 0xFF, 0xFF]

--    it "should encode (9223372036854775807 :: Int) as Ubjson int64" $ do
--      let actual = (B.unpack $ encode (9223372036854775807 :: Int))
--      actual `shouldBe` expected [ 0x4C, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00 ]

    -- TODO add more tests for huge numbers


    -- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

    it "should encode (-NaN :: Float) as Ubjson float32" $ do
      let actual = B.unpack $ encode (read "-NaN" :: Float)
      actual `shouldBe` expected [0x64, 0x7F, 0xC0, 0x00, 0x00]

    it "should encode (-Infinity :: Float) as Ubjson float32" $ do
      let actual = B.unpack $ encode (read "-Infinity" :: Float)
      actual `shouldBe` expected [0x64, 0xFF, 0x80, 0x00, 0x00]

    it "should encode (0 :: Float) as Ubjson float32" $ do
      let actual = B.unpack $ encode (0 :: Float)
      actual `shouldBe` expected [0x64, 0x00, 0x00, 0x00, 0x00]

    it "should encode (0.1 :: Float) as Ubjson float32" $ do
      let actual = B.unpack $ encode (0.1 :: Float)
      actual `shouldBe` expected [0x64, 0x3d, 0xcc, 0xcc, 0xcd]
      -- value actually represented is 0.100000001490116119384765625

    it "should encode (Infinity :: Float) as Ubjson float32" $ do
      let actual = B.unpack $ encode (read "Infinity" :: Float)
      actual `shouldBe` expected [0x64, 0x7F, 0x80, 0x00, 0x00 ]

    it "should encode (NaN :: Float) as Ubjson float32" $ do
      let actual = B.unpack $ encode (read "NaN" :: Float)
      actual `shouldBe` expected [0x64, 0xFF, 0xC0, 0x00, 0x00]

  -- TODO Add more tests for doubles


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- strings
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode ['a', 'b', 'c'] as Ubjson string" $ do
      let actual = B.unpack $ encode ['a','b','c']
      --                             S     U     3     a     b      c
      actual `shouldBe` expected [0x53, 0x55, 0x03, 0x61, 0x62, 0x63]

    it "should encode (\"hello\" :: Text) as Ubjson string" $ do
      let actual = B.unpack $ encode (T.pack "hello")
      --                             S     U     5     h     e     l     l     0
      actual `shouldBe` expected [0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F]


    it "should encode (\"привет\" :: Text) as Ubjson string" $ do
      let actual = B.unpack $ encode (T.pack "привет")
      --                             S     U    12           п           р           и           в           е           т
      actual `shouldBe` expected [0x53, 0x55, 0x0C, 0xD0, 0xBF, 0xD1, 0x80, 0xD0, 0xB8, 0xD0, 0xB2, 0xD0, 0xB5, 0xD1, 0x82]


    it "should encode (\"مرحبا\" :: Text) as Ubjson string" $ do
      let actual = B.unpack $ encode (T.pack "مرحبا")
      --                             S     U    10           ا            ب           ح           ر           م
      actual `shouldBe` expected [0x53, 0x55, 0x0A, 0xD9, 0x85, 0xD8, 0xB1, 0xD8, 0xAD, 0xD8, 0xA8, 0xD8, 0xA7]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Maybe, Nothing and Just
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode (Nothing :: Maybe) as Ubjson null" $ do
      let actual = B.unpack $ encode (Nothing :: Maybe Bool)
      actual `shouldBe` expected [0x5A]


    it "should encode (Just 'c' :: Maybe) as Ubjson char" $ do
      let actual = B.unpack $ encode (Just 'c')
      actual `shouldBe` expected [0x43, 0x63]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Lists of homogeneous chars, integrals, booleans, etc.
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode ([163, -93, 256 :: Int]) as Ubjson array" $ do
      let actual = B.unpack $ encode ([163, -93, 256 :: Int])
      actual `shouldBe` expected [0x5B, 0x55, 0xA3, 0x69, 0xA3, 0x49, 0x01, 0x00, 0x5D]

    it "should encode [True, False, True, True] as Ubjson array" $ do
      let actual = B.unpack $ encode [True, False, True, True]
      actual `shouldBe` expected [0x5B, 0x54, 0x46, 0x54, 0x54, 0x5D]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Lists of heterogeneous values
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode heterogeneous list as Ubjson array" $ do
      let nothing = Nothing :: Maybe Bool
      let hlist = [Ubj Noop, Ubj nothing, Ubj True, Ubj 'a', Ubj (163 :: Int), Ubj "hello"]
      let actual = B.unpack $ encode hlist
      actual `shouldBe` expected [0x5B, 0x4E, 0x5A, 0x54, 0x43, 0x61, 0x55, 0xA3, 0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x5D]


    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Maps
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    it "should encode M.fromList [(\"age\", 42 :: Int)] as Ubjson object" $ do
      let m = M.fromList [("age", 42 :: Int)]
      let actual = B.unpack $ encode m
      actual `shouldBe` expected [0x7B, 0x55, 0x03, 0x61, 0x67, 0x65, 0x55, 0x2A, 0x7D]




