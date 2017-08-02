{-# LANGUAGE
		OverloadedStrings
  , ExistentialQuantification
  , FlexibleInstances
#-}

module Data.Ubjson (
    encode
  , Noop (..)
  , Null (..)
  , Ubj  (..)
  )
where

--import           Prelude
import qualified Data.ByteString.Lazy    as L
import           Data.ByteString.Builder --(Builder, toLazyByteString, charUtf8, word8, int8, int16BE, int32BE, int64BE)
import           Data.Int                --(Int8, Int16)
import qualified Data.Map                as M
import           Data.Monoid             --((<>))
import qualified Data.Text               as T
import           Data.Text.Encoding      --(encodeUtf8Builder)
import           Data.Word               --(Word8)


data Noop = Noop

data Null = Null

class ToUbjson a where
  toEncoding :: a -> Builder

instance ToUbjson Null where
  toEncoding Null = charUtf8 'Z'

instance ToUbjson Noop where
  toEncoding Noop = charUtf8 'N'

instance ToUbjson Bool where
  toEncoding True  = charUtf8 'T'
  toEncoding False = charUtf8 'F'

instance ToUbjson Char where
  toEncoding x = charUtf8 'C' <> charUtf8 x

instance ToUbjson Word8 where
  toEncoding x = charUtf8 'U' <> word8 x

instance ToUbjson Int8 where
  toEncoding x = charUtf8 'i' <> int8 x

instance ToUbjson Int16 where
  toEncoding x = charUtf8 'I' <> int16BE x

instance ToUbjson Int32 where
  toEncoding x = charUtf8 'l' <> int32BE x

instance ToUbjson Int64 where
  toEncoding x = charUtf8 'L' <> int64BE x

instance ToUbjson Int where
  toEncoding x
    | x >= 0     && x <= 2^8  -1 = toEncoding ((fromIntegral x) :: Word8)
    | x >= -2^7  && x <= 2^7  -1 = toEncoding ((fromIntegral x) :: Int8)
    | x >= -2^15 && x <= 2^15 -1 = toEncoding ((fromIntegral x) :: Int16)
    | x >= -2^31 && x <= 2^31 -1 = toEncoding ((fromIntegral x) :: Int32)
    | x >= -2^63 && x <= 2^63 -1 = toEncoding ((fromIntegral x) :: Int64)
    | otherwise                  = undefined


textToEncoding :: T.Text -> Builder
textToEncoding x =
  let b = encodeUtf8Builder x
      l = (fromIntegral $ L.length $ toLazyByteString b) :: Int
  in toEncoding l <> b


instance ToUbjson T.Text where
  toEncoding x = charUtf8 'S' <> textToEncoding x


instance ToUbjson a => ToUbjson (Maybe a) where
  toEncoding x = case x of
    Nothing -> charUtf8 'Z'
    Just y  -> toEncoding y


instance (ToUbjson a) => ToUbjson [a] where
  toEncoding [] = charUtf8 '[' <> charUtf8 ']'
  toEncoding xs = charUtf8 '[' <> mconcat (map toEncoding xs) <> charUtf8 ']'

-- TODO Try to optimize "mconcat (map toEncoding xs)" with a foldl

data Ubj = forall a. ToUbjson a => Ubj a


instance ToUbjson Ubj where
  toEncoding (Ubj a) = toEncoding a

-- type Dict v = M.Map T.Text v

-- instance ToUbjson v => ToUbjson (Dict v) where
instance ToUbjson v => ToUbjson (M.Map T.Text v) where
  toEncoding m =
    let s = charUtf8 '{'
        f b k v = b <> textToEncoding k <> toEncoding v
        e = charUtf8 '}'
    in (M.foldlWithKey f s m) <> e
    -- TODO How about invoking strict fold? Why left and not right?


encode :: ToUbjson a => a -> L.ByteString
encode x = toLazyByteString $ toEncoding x
