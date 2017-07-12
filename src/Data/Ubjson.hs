module Data.Ubjson (encode, pack) where

import qualified Data.ByteString.Lazy    as L
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Word
import           Data.Int

class Ubjson a where
  pack :: a -> Builder

instance Ubjson Bool where
  pack True  = charUtf8 'T'
  pack False = charUtf8 'F'

instance Ubjson Char where
  pack a = charUtf8 'C' <> charUtf8 a

instance Ubjson Word8 where
  pack a = charUtf8 'U' <> word8 a

instance Ubjson Int8 where
  pack a = charUtf8 'i' <> int8 a

-- TODO Num types here ...

instance Ubjson Integer where
  pack a
    | a >= 0     && a <= 2^8  -1 = pack ((fromIntegral a) :: Word8)
    | a >= -2^7  && a <= 2^7  -1 = pack ((fromIntegral a) :: Int8)
--    | a >= -2^15 && a <= 2^15 -1 = charUtf8 'I' <> int16 ((fromIntegral a) :: Int16)
    | otherwise               = undefined


-- TODO instance Ubjson Text where

instance Ubjson a => Ubjson (Maybe a) where
  pack b = case b of
    Nothing -> charUtf8 'Z'
    Just a  -> pack a

--instance Ubjson a => Ubjson [a] where =>
--  pack []     = charUtf8 '[' <> charUtf8 ']'
--  pack (x:xs) = pack x <> pack xs
-- foldr <> 0

encode :: Ubjson a => a -> L.ByteString
encode a = toLazyByteString $ pack a



