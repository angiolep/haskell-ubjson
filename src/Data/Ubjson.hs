{-# LANGUAGE
    OverloadedStrings
  , ExistentialQuantification
  , FlexibleInstances
#-}

{-|
Module      : Data.Ubjson
Description : UBJSON codec
Copyright   : (c) Paolo Angioletti, 2017
                  BitSpoke Ltd, 2017
License     : BSD-3
Maintainer  : paolo.angioletti@gmail.com
Stability   : experimental
Portability : POSIX

This package provides functions to __encode__ (__decode__) some of the most common
Haskell data types to (from)  UBJSON - Universal Binary JSON strings.

Please refer to the official UBJSON specification published at http://ubjson.org/
for further details about the binary format.
-}
module Data.Ubjson (
  -- * Encoding
  -- $encoding

    encode
  , ToUbjson (..)
  -- ** Booleans
  -- $booleans

  -- ** Characters
  -- $characters

  -- ** Numbers
  -- $numbers

  -- ** Strings
  -- $strings

  -- ** Nulls
  -- $nulls

  -- ** Containers
  -- $containers

  -- *** Arrays
  -- $arrays

  -- *** Objects
  -- $objects

  -- * Decoding
  -- $decoding

  , decode

  -- * Types
  , Noop (..)
  , Ubj  (..)
  )
where


import qualified Data.ByteString.Lazy    as L
import           Data.ByteString.Builder --(Builder, toLazyByteString, charUtf8, word8, int8, int16BE, int32BE, int64BE, floatBE, doubleBE)
import           Data.Int                --(Int8, Int16)
import           Data.Map                (Map, fromList, foldlWithKey)
import           Data.Monoid             --((<>))
import qualified Data.String             as S
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as TE
import           Data.Word               --(Word8)


-- | Encode a Haskell value to an UBJSON binary string as a lazy 'L.ByteString'.
-- The input value is of a data type constrained to the `ToUbjson` type class because
-- serialization is performed by applying the 'serialize' method of that class.
encode :: ToUbjson a => a -> L.ByteString
encode x = toLazyByteString $ serialize x

decode :: L.ByteString -> a
decode = undefined

-- | Class of types that can be serialized to UBJSON binary strings
class ToUbjson a where
  -- | Method which serializes the given input value to the most appropriate UBJSON
  -- binary string
  serialize :: a -> Builder

-- | A Ubjson \"no operation\" value
data Noop = Noop

instance ToUbjson Noop where
  serialize Noop = charUtf8 'N'

instance ToUbjson Bool where
  serialize True  = charUtf8 'T'
  serialize False = charUtf8 'F'

instance ToUbjson Char where
  serialize x = charUtf8 'C' <> charUtf8 x

instance ToUbjson Word8 where
  serialize x = charUtf8 'U' <> word8 x

instance ToUbjson Int8 where
  serialize x = charUtf8 'i' <> int8 x

instance ToUbjson Int16 where
  serialize x = charUtf8 'I' <> int16BE x

instance ToUbjson Int32 where
  serialize x = charUtf8 'l' <> int32BE x

instance ToUbjson Int64 where
  serialize x = charUtf8 'L' <> int64BE x

instance ToUbjson Int where
  serialize x
    | x >= 0     && x <= 2^8  -1 = serialize ((fromIntegral x) :: Word8)
    | x >= -2^7  && x <= 2^7  -1 = serialize ((fromIntegral x) :: Int8)
    | x >= -2^15 && x <= 2^15 -1 = serialize ((fromIntegral x) :: Int16)
    | x >= -2^31 && x <= 2^31 -1 = serialize ((fromIntegral x) :: Int32)
    | x >= -2^63 && x <= 2^63 -1 = serialize ((fromIntegral x) :: Int64)
    | otherwise                  = undefined

-- TODO encode high-precision integrals

instance ToUbjson Float where
  serialize x = charUtf8 'd' <> floatBE x

instance ToUbjson Double where
  serialize x = charUtf8 'D' <> doubleBE x


stringToText :: String -> Text
stringToText str = S.fromString str :: Text

textToEncoding :: Text -> Builder
textToEncoding txt =
  let enc = TE.encodeUtf8Builder $ txt
      len = fromIntegral $ L.length $ toLazyByteString enc :: Int
  in serialize len <> enc

stringToEncoding = textToEncoding . stringToText

instance {-# OVERLAPPING #-} ToUbjson String where
  serialize str = charUtf8 'S' <> stringToEncoding str

instance ToUbjson Text where
  serialize txt = charUtf8 'S' <> textToEncoding txt


instance ToUbjson a => ToUbjson (Maybe a) where
  serialize x = case x of
    Nothing -> charUtf8 'Z'
    Just y  -> serialize y


instance {-# OVERLAPPABLE #-} (ToUbjson a) => ToUbjson [a] where
  serialize [] = charUtf8 '[' <> charUtf8 ']'
  serialize xs = charUtf8 '[' <> mconcat (map serialize xs) <> charUtf8 ']'

-- TODO Try to optimize "mconcat (map serialize xs)" with a foldl

-- | A wrapper to make heterogeneous list of values
data Ubj = forall a. ToUbjson a => Ubj a


instance ToUbjson Ubj where
  serialize (Ubj a) = serialize a

type Dict v = Map String v

instance ToUbjson v => ToUbjson (Dict v) where
  serialize m =
    let s = charUtf8 '{'
        f b k v = b <> stringToEncoding k <> serialize v
        e = charUtf8 '}'
    in (foldlWithKey f s m) <> e
    -- TODO How about invoking strict fold? Why left and not right?



-- $encoding
-- The 'encode' function is polymorphic and can be applied to any data type which
-- provides and instance of the 'ToUbjson' type class


-- $booleans
-- This package provides instances of the 'ToUbjson' class for the Haskell 'Bool' data
-- type so that 'True' and 'False' can be directly encoded to a binary strings of
-- [UBJSON bool type](http://ubjson.org/type-reference/value-types/#bool)
-- as follows:
--
-- >>> encode True
-- [0x54]
--
-- >>> encode False
-- [0x46]


-- $characters
-- This package provides instances of the 'ToUbjson' class for the Haskell 'Char'
-- data type so that characters can be directly encoded to a binary strings of
-- [UBJSON char type](http://ubjson.org/type-reference/value-types/#char)
-- as follows:
--
-- >>> encode 'h'
-- [0x43, 0x68]
--

-- $numbers
-- This package provides instances of the 'ToUbjson' class for the most common
-- Haskell numerical data types so that numbers can be directly encoded to a binary
-- string of the most appropriate
-- [UBJSON numeric type]((http://ubjson.org/type-reference/value-types/#numeric)
-- as follows:
--
-- >>> encode (163 :: Int)
-- [0x55, 0xA3]
--
-- >>> encode (-93 :: Int)
-- [0x69, 0xA3]
--
-- >>> encode (256 :: Int)
-- [0x49, 0x01, 0x00]
--
-- >>> encode (32768 :: Int)
-- [0x6C, 0x00, 0x00, 0x80, 0x00]
--
-- For integral numbers, the resulting UBJSON type, such as @uint8@, @int8@, @int16@,
-- etc. does not depend on the specific Haskell data type but rather on how big the
-- value being serialized is.
--
--     * @uint8@ for numbers between 0 and (2^8 - 1)
--
--     * @int8@ for numbers between -(2^7) and 0
--
--     * @int16@ for numbers between -(2^15) and -(2^7 - 1), and for numbers between
--       2^8 and (2^15 - 1)
--
--     * @int32@ for numbers between -(2^31) and -(2^15 - 1), and for numbers between
--       2^15 and (2^31 - 1)
--
--     * @int64@ for numbers between -(2^63) and -(2^31 - 1), and for numbers between
--       2^31 and (2^64 - 1)
--
-- For fractional numbers, the resulting UBJSON type is either @float32@ or @float64@
-- depending on the specific Haskell data type of the value being serialized which
-- could be 'Float' or 'Double'
--
-- >>> encode (0.1 :: Float)
-- [0x64, 0x3d, 0xcc, 0xcc, 0xcd]
--
-- >>> encode (read "Infinity" :: Float)
-- [0x64, 0x7F, 0x80, 0x00, 0x00 ]


-- $strings
-- This package provides instances of the 'ToUbjson' class for the most common Haskell
-- string-like data types (e.g. list of 'Char', 'Text', etc.) so that text can be
-- directly encoded to a binary string of the
-- [UBJSON string type](http://ubjson.org/type-reference/value-types/#string-encoding)
-- as follows:
--
-- >>> encode ['h', 'e', 'l', 'l', 'o']
-- [0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F]
--
-- >>> encode "hello"
-- [0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F]
--
-- >>> import Data.Text (pack)
-- >>> encode (pack "привет")
-- [0x53, 0x55, 0x0C, 0xD0, 0xBF, 0xD1, 0x80, 0xD0, 0xB8, 0xD0, 0xB2, 0xD0, 0xB5, 0xD1, 0x82]
--
-- >>> encode (pack "مرحبا")
-- [0x53, 0x55, 0x0A, 0xD9, 0x85, 0xD8, 0xB1, 0xD8, 0xAD, 0xD8, 0xA8, 0xD8, 0xA7]
--

-- $nulls
-- This package provides instances of the 'ToUbjson' class for the Haskel 'Maybe'
-- data type so that optional values can be directly encoded either to the
-- [UBJSON null value](http://ubjson.org/type-reference/value-types/#null) or to the
-- most appropriate else UBJSON value as follows
--
-- >>> encode (Nothing :: Maybe String)
-- [0x5A]
--
-- >>> encode (Just "hello")
-- [0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F]


-- $containers
-- UBJSON containers are either arrays or objects.
-- Please refer to the official specification for further details about
-- [UBJSON containers types](http://ubjson.org/type-reference/container-types/)

-- $arrays
-- This package provides instances of the 'ToUbjson' class for __homogeneous__
-- Haskell lists of any serializable Haskell data type so that they can be directly
-- encoded to binary strings of
-- [UBJSON array type](http://ubjson.org/type-reference/container-types/#array)
-- as follows:
--
-- >>> encode [163, -93, 256 :: Int]
-- [0x5B, 0x55, 0xA3, 0x69, 0xA3, 0x49, 0x01, 0x00, 0x5D]
--
-- >>> encode [True, False, True, True]
-- [0x5B, 0x54, 0x46, 0x54, 0x54, 0x5D]
--
-- Please note that Haskell list of characters will NOT be encoded to UBJSON arrays
-- but rather to UBJSON strings as documented above.
--
-- You can also make __heterogenous__ Haskell lists by wrapping values via the 'Ubj'
-- data constructor and then encode as follows:
--
-- >>> let nothing = Nothing :: Maybe Bool
-- >>> encode [Ubj Noop, Ubj nothing, Ubj True, Ubj 'a', Ubj (163 :: Int), Ubj "hello"]
-- [0x5B, 0x4E, 0x5A, 0x54, 0x43, 0x61, 0x55, 0xA3, 0x53, 0x55, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x5D]
--
--

-- $objects
-- This package provides instances of the 'ToUbjson' class for the Haskell 'Map' data
-- type so that structures of key/value pairs can be directly encoded to binary
-- strings of
-- [UBJSON object type](http://ubjson.org/type-reference/container-types/#object)
-- as follows:
--
-- >>> import Data.Map (fromList)
-- >>> encode $ fromList [("age", 42 :: Int)]
-- [0x7B, 0x55, 0x03, 0x61, 0x67, 0x65, 0x55, 0x2A, 0x7D]

-- $decoding
-- The 'decode' function is still undefined. It will be provided next release.

