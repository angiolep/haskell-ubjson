module Data.Ubjson (
    encode
  , Noop (..)
  )
where

--import           Prelude
import qualified Data.Text               as T
import           Data.Text.Encoding      --(encodeUtf8Builder)
import qualified Data.ByteString.Lazy    as L
import           Data.ByteString.Builder --(Builder, toLazyByteString, charUtf8, word8, int8, int16BE, int32BE, int64BE)
import           Data.Monoid             --((<>))
import           Data.Word               --(Word8)
import           Data.Int                --(Int8, Int16)


data Noop = Noop

class Ubjson a where
  pack :: a -> Builder

instance Ubjson Noop where
  pack Noop = charUtf8 'N'

instance Ubjson Bool where
  pack True  = charUtf8 'T'
  pack False = charUtf8 'F'

instance Ubjson Char where
  pack x = charUtf8 'C' <> charUtf8 x

instance Ubjson Word8 where
  pack x = charUtf8 'U' <> word8 x

instance Ubjson Int8 where
  pack x = charUtf8 'i' <> int8 x

instance Ubjson Int16 where
  pack x = charUtf8 'I' <> int16BE x

instance Ubjson Int32 where
  pack x = charUtf8 'l' <> int32BE x

instance Ubjson Int64 where
  pack x = charUtf8 'L' <> int64BE x

instance Ubjson Int where
  pack x
    | x >= 0     && x <= 2^8  -1 = pack ((fromIntegral x) :: Word8)
    | x >= -2^7  && x <= 2^7  -1 = pack ((fromIntegral x) :: Int8)
    | x >= -2^15 && x <= 2^15 -1 = pack ((fromIntegral x) :: Int16)
    | x >= -2^31 && x <= 2^31 -1 = pack ((fromIntegral x) :: Int32)
    | x >= -2^63 && x <= 2^63 -1 = pack ((fromIntegral x) :: Int64)
    | otherwise                  = undefined


instance Ubjson T.Text where
  pack x =
    let builder = encodeUtf8Builder x
        len = (fromIntegral $ L.length $ toLazyByteString builder) :: Int
    in charUtf8 'S' <> pack len <> builder

instance Ubjson a => Ubjson (Maybe a) where
  pack x = case x of
    Nothing -> charUtf8 'Z'
    Just y  -> pack y

instance (Ubjson a) => Ubjson [a] where
  pack [] = charUtf8 '[' <> charUtf8 ']'
  pack xs = charUtf8 '[' <> mconcat (map pack xs) <> charUtf8 ']'


encode :: Ubjson a => a -> L.ByteString
encode x = toLazyByteString $ pack x



