{-# LANGUAGE ExistentialQuantification #-}

module Data.Ubjson (
    encode
  , Noop (..)
  , Null (..)
  , Ubj  (..)
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
import           Data.Dynamic


data Noop = Noop

data Null = Null

class ToUbjson a where
  pack :: a -> Builder

instance ToUbjson Null where
  pack Null = charUtf8 'Z'

instance ToUbjson Noop where
  pack Noop = charUtf8 'N'

instance ToUbjson Bool where
  pack True  = charUtf8 'T'
  pack False = charUtf8 'F'

instance ToUbjson Char where
  pack x = charUtf8 'C' <> charUtf8 x

instance ToUbjson Word8 where
  pack x = charUtf8 'U' <> word8 x

instance ToUbjson Int8 where
  pack x = charUtf8 'i' <> int8 x

instance ToUbjson Int16 where
  pack x = charUtf8 'I' <> int16BE x

instance ToUbjson Int32 where
  pack x = charUtf8 'l' <> int32BE x

instance ToUbjson Int64 where
  pack x = charUtf8 'L' <> int64BE x

instance ToUbjson Int where
  pack x
    | x >= 0     && x <= 2^8  -1 = pack ((fromIntegral x) :: Word8)
    | x >= -2^7  && x <= 2^7  -1 = pack ((fromIntegral x) :: Int8)
    | x >= -2^15 && x <= 2^15 -1 = pack ((fromIntegral x) :: Int16)
    | x >= -2^31 && x <= 2^31 -1 = pack ((fromIntegral x) :: Int32)
    | x >= -2^63 && x <= 2^63 -1 = pack ((fromIntegral x) :: Int64)
    | otherwise                  = undefined


instance ToUbjson T.Text where
  pack x =
    let builder = encodeUtf8Builder x
        len = (fromIntegral $ L.length $ toLazyByteString builder) :: Int
    in charUtf8 'S' <> pack len <> builder

instance ToUbjson a => ToUbjson (Maybe a) where
  pack x = case x of
    Nothing -> charUtf8 'Z'
    Just y  -> pack y

instance (ToUbjson a) => ToUbjson [a] where
  pack [] = charUtf8 '[' <> charUtf8 ']'
  pack xs = charUtf8 '[' <> mconcat (map pack xs) <> charUtf8 ']'


data Ubj = forall a. ToUbjson a => Ubj a

instance ToUbjson Ubj where
  pack (Ubj a) = pack a


encode :: ToUbjson a => a -> L.ByteString
encode x = toLazyByteString $ pack x
