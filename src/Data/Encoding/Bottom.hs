{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Data.Encoding.Bottom
  ( Bottom,
    unBottom,
    encode,
    decode,
    decode',
  )
where

import Control.Monad (void)
import Data.Bits (zeroBits)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (Parsec, chunk, eof, errorBundlePretty, runParser, someTill, (<|>))

-- Bottom is just a wrapper around a ByteString.
newtype Bottom = Bottom ByteString
  deriving (Show, Eq, Ord, Semigroup, Monoid)

unBottom :: Bottom -> ByteString
unBottom (Bottom bs) = bs

-- Value characters for encoding.
singleton :: Char -> ByteString
singleton = encodeUtf8 . T.singleton

twoHundred :: ByteString
twoHundred = singleton '\x1FAC2'

fifty :: ByteString
fifty = singleton '\x1F496'

ten :: ByteString
ten = singleton '\x2728'

five :: ByteString
five = singleton '\x1F97A'

one :: ByteString
one = singleton '\x002C'

zero :: ByteString
zero = singleton '\x2764'

separator :: ByteString
separator = encodeUtf8 $ T.pack ['\x1F449', '\x1F448']

-- Decoding functions.
decode :: Bottom -> Text
decode (Bottom bs) = case decode' bs of
  Right r -> r
  Left err -> error "Data.Encoding.Bottom.decode: malformed Bottom: " <> err

type Parser = Parsec Void ByteString

decode' :: ByteString -> Either Text Text
decode' bs = case runParser bottomParser "" bs of
  Left err -> Left $ T.pack $ errorBundlePretty err
  Right r -> Right r
  where
    bottomParser :: Parser Text
    bottomParser = decodeUtf8 . BS.pack <$> (fmap . fmap) toEnum (groupParser `someTill` eof)

    groupParser :: Parser Int
    groupParser = parseNull <|> parseValues
      where
        parseNull = zeroParser >> separatorParser >> return 0
        parseValues = sum <$> (twoHundredParser <|> fiftyParser <|> tenParser <|> fiveParser <|> oneParser) `someTill` separatorParser

    twoHundredParser :: Parser Int
    twoHundredParser = chunk twoHundred >> return 200

    fiftyParser :: Parser Int
    fiftyParser = chunk fifty >> return 50

    tenParser :: Parser Int
    tenParser = chunk ten >> return 10

    fiveParser :: Parser Int
    fiveParser = chunk five >> return 5

    oneParser :: Parser Int
    oneParser = chunk one >> return 1

    zeroParser :: Parser Int
    zeroParser = chunk zero >> return 0

    separatorParser :: Parser ()
    separatorParser = void $ chunk separator

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
