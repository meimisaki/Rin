module Data.Buffer
( Buffer
, Word8
, mkBuffer
, offsetBytes
, prevByte
, nextByte
, lexeme
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Word

data Buffer = Buffer
  { offset :: Int
  , bytes :: BS.ByteString }

mkBuffer :: String -> Buffer
mkBuffer str = Buffer
  { offset = 0
  , bytes = C8.pack str }

offsetBytes :: Int -> Buffer -> Buffer
offsetBytes n buf = buf { offset = offset buf + n }

prevByte :: Buffer -> Maybe (Word8, Buffer)
prevByte buf
  | offset buf <= 0 = Nothing
  | otherwise = Just (byte, buf1)
    where byte = bytes buf `BS.index` offset buf - 1
          buf1 = buf { offset = offset buf - 1 }

nextByte :: Buffer -> Maybe (Word8, Buffer)
nextByte buf
  | offset buf >= BS.length (bytes buf) = Nothing
  | otherwise = Just (byte, buf1)
    where byte = bytes buf `BS.index` offset buf
          buf1 = buf { offset = offset buf + 1 }

lexeme :: Buffer -> Int -> String
lexeme buf len = C8.unpack (BS.take len (BS.drop (offset buf) (bytes buf)))
