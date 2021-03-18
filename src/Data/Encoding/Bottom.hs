module Data.Encoding.Bottom (Bottom, asText, encode, decode, decode') where

import Data.Bits (zeroBits)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import Data.List (unfoldr)
import Data.Maybe (fromJust)

-- Bottom is just a wrapper around a ByteString.
newtype Bottom = Bottom ByteString
  deriving (Show, Eq)

asText :: Bottom -> Text
asText (Bottom t) = decodeUtf8 t

-- Constants for encoding.
twoHundred :: ByteString
twoHundred = encodeUtf8 $ T.singleton '\x1FAC2'

fifty :: ByteString
fifty = encodeUtf8 $ T.singleton '\x1F496'

ten :: ByteString
ten = encodeUtf8 $ T.singleton '\x2728'

five :: ByteString
five = encodeUtf8 $ T.singleton '\x1F97A'

one :: ByteString
one = encodeUtf8 $ T.singleton '\x002C'

zero :: ByteString
zero = encodeUtf8 $ T.singleton '\x2764'

separator :: ByteString
separator = encodeUtf8 $ T.pack ['\x1F449', '\x1F448']

-- Decoding functions.
decode :: Bottom -> Text
decode (Bottom bs) = fromJust $ decode' bs

decode' :: ByteString -> Maybe Text
decode' = undefined

-- Encoding functions.
encode :: Text -> Bottom
encode = Bottom . BS.concatMap encodeByte . encodeUtf8

encodeByte :: Word8 -> ByteString
encodeByte b
  | b == zeroBits = zero
  | otherwise = BS.concat (unfoldr encodeByte' (fromEnum b)) <> separator
  where
    encodeByte' :: Int -> Maybe (ByteString, Int)
    encodeByte' n
      | n >= 200 = Just (twoHundred, n - 200)
      | n >= 50 = Just (fifty, n - 50)
      | n >= 10 = Just (ten, n - 10)
      | n >= 5 = Just (five, n - 5)
      | n >= 1 = Just (one, n - 1)
      | n == 0 = Nothing
      | otherwise = error "Data.Encoding.Bottom.encodeByte': impossible: unsigned byte is negative"
