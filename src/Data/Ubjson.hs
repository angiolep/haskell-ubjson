module Data.Ubjson (encode) where

--import           Prelude
import qualified Data.Text               as T
import           Data.Text.Encoding      --(encodeUtf8Builder)
import qualified Data.ByteString.Lazy    as L
import           Data.ByteString.Builder --(Builder, toLazyByteString, charUtf8, int8, word8)
import           Data.Monoid             --((<>))
import           Data.Word               --(Word8)
import           Data.Int                --(Int8)

class Ubjson a where
  pack :: a -> Builder

instance Ubjson Bool where
  pack True  = charUtf8 'T'
  pack False = charUtf8 'F'

instance Ubjson Char where
  pack x = charUtf8 'C' <> charUtf8 x

instance Ubjson Word8 where
  pack x = charUtf8 'U' <> word8 x

instance Ubjson Int8 where
  pack x = charUtf8 'i' <> int8 x

-- TODO Num types here ...

instance Ubjson Int where
  pack x
    | x >= 0     && x <= 2^8  -1 = pack ((fromIntegral x) :: Word8)
    | x >= -2^7  && x <= 2^7  -1 = pack ((fromIntegral x) :: Int8)
--    | x >= -2^15 && x <= 2^15 -1 = charUtf8 'I' <> int16 ((fromIntegral x) :: Int16)
    | otherwise               = undefined


instance Ubjson T.Text where
  pack x =
    let len = T.length x
    in charUtf8 'S' <> pack len <> encodeUtf8Builder x

instance Ubjson a => Ubjson (Maybe a) where
  pack x = case x of
    Nothing -> charUtf8 'Z'
    Just y  -> pack y

instance (Ubjson a) => Ubjson [a] where
  pack []     = undefined
  pack (x:xs) = pack x <> pack xs


encode :: Ubjson a => a -> L.ByteString
encode x = toLazyByteString $ pack x



