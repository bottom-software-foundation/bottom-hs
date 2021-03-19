{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Encoding.Bottom (decode', encode, unBottom)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec (Expectation, describe, hspec, it, shouldBe)

-- Why does this cause GHC to return an error? Related to https://gitlab.haskell.org/ghc/ghc/-/issues/5518?
twoHundred :: Text
twoHundred = T.singleton '\129730'

testEncode :: Text -> Text -> Expectation
testEncode input expected = unBottom (encode input) `shouldBe` encodeUtf8 expected

testDecode :: Text -> Text -> Expectation
testDecode input expected = case decode' $ encodeUtf8 input of
  Right r -> encodeUtf8 r `shouldBe` encodeUtf8 expected
  Left err -> error $ T.unpack err

testCases :: [(Text, Text)]
testCases =
  [ ("Test", "💖✨✨✨,,,,👉👈💖💖,👉👈💖💖✨🥺👉👈💖💖✨🥺,👉👈"),
    ("h", "💖💖,,,,👉👈"),
    ("🥺", twoHundred <> "✨✨✨✨👉👈💖💖💖🥺,,,,👉👈💖💖💖✨🥺👉👈💖💖💖✨✨✨🥺,👉👈"),
    ( "がんばれ",
      twoHundred
        <> "✨✨🥺,,👉👈💖💖✨✨🥺,,,,👉👈💖💖✨✨✨✨👉👈"
        <> twoHundred
        <> "✨✨🥺,,👉👈💖💖✨✨✨👉👈💖💖✨✨✨✨🥺,,👉👈"
        <> twoHundred
        <> "✨✨🥺,,👉👈💖💖✨✨🥺,,,,👉👈💖💖💖✨✨🥺,👉👈"
        <> twoHundred
        <> "✨✨🥺,,👉👈💖💖✨✨✨👉👈💖💖✨✨✨✨👉👈"
    )
  ]

main :: IO ()
main = hspec $ do
  -- Taken from https://github.com/bottom-software-foundation/bottom-rs/blob/need_top/src/bottom.rs
  describe "Data.Encoding.Bottom" $ do
    it "encodes strings" $ sequence_ $ uncurry testEncode <$> testCases
    it "decodes strings" $ sequence_ $ uncurry (flip testDecode) <$> testCases
