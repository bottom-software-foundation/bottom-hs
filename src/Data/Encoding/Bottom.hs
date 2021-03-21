{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Encodes and decodes 'Text's to 'Bottom's. For details, see the
-- [Bottom spec](https://github.com/bottom-software-foundation/spec).
module Data.Encoding.Bottom
  ( Bottom,
    unBottom,
    encode,
    decode,
    decode',
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (void)
import Data.Bits (zeroBits)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (Parsec, Token, chunk, eof, manyTill, runParser, someTill, (<|>))
import Text.Megaparsec.Error (ErrorItem (..), ParseError (..), ParseErrorBundle (..), errorBundlePretty)

-- | A 'Bottom' is a wrapper around well-formed, Bottom-encoded 'ByteString'.
-- Its instances are derived from those of 'ByteString'.
newtype Bottom = Bottom ByteString
  deriving (Show, Eq, Ord, Semigroup, Monoid, NFData)

-- | 'unBottom' unwraps the underlying 'ByteString'.
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
zero = encodeUtf8 $ T.pack ['\x2764', '\xFE0F']

separator :: ByteString
separator = encodeUtf8 $ T.pack ['\x1F449', '\x1F448']

-- Decoding functions.

-- | 'decode' decodes a 'Bottom' into its corresponding Unicode 'Text'.
decode :: Bottom -> Text
decode (Bottom bs) = case decode' bs of
  Right r -> r
  Left err -> error "Data.Encoding.Bottom.decode: malformed Bottom: " <> err

type Parser = Parsec Void ByteString

-- | 'decode'' decodes an arbitrary Bottom-encoded 'ByteString' into a 'Text',
-- or returns a parse error if the 'ByteString' is malformed.
decode' :: ByteString -> Either Text Text
decode' bs = case runParser bottomParser "" bs of
  Left err -> Left $ renderError err
  Right r -> Right r
  where
    bottomParser :: Parser Text
    bottomParser = decodeUtf8 . BS.pack <$> (fmap . fmap) toEnum (groupParser `manyTill` eof)

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

    -- Custom error messages, because the default error messages print raw
    -- ByteStrings and don't render correctly.
    renderError :: ParseErrorBundle ByteString Void -> Text
    renderError ParseErrorBundle {bundleErrors = (TrivialError offset unexpected expected) :| []} =
      unexpectedMessage <> expectedMessage <> " at offset " <> T.pack (show offset)
      where
        renderErrorItem :: ErrorItem (Token ByteString) -> Text
        renderErrorItem (Tokens tokens) = "\"" <> decodeUtf8 (BS.pack $ NE.toList tokens) <> "\""
        renderErrorItem (Label name) = T.pack $ NE.toList name
        renderErrorItem EndOfInput = "end of input"

        unexpectedMessage = case unexpected of
          Just unx -> "unexpected " <> renderErrorItem unx <> ": "
          Nothing -> ""

        expectedMessage = "expected " <> (if length expecteds >= 2 then "one of " else "") <> T.intercalate ", " expecteds
          where
            expecteds = renderErrorItem <$> F.toList expected
    renderError err = T.pack $ errorBundlePretty err

-- Encoding functions.

-- | 'encode' takes a 'Text', and encodes it into a 'Bottom'. To get at the
-- underlying 'ByteString', unwrap the returned value with 'unBottom'.
encode :: Text -> Bottom
encode = Bottom . BS.concatMap encodeByte . encodeUtf8

encodeByte :: Word8 -> ByteString
encodeByte b
  | b == zeroBits = zero <> separator
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
