{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Encoding.Bottom (encode, unBottom)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec (Expectation, describe, hspec, it, shouldBe)

-- Why does this cause GHC to return an error? Related to https://gitlab.haskell.org/ghc/ghc/-/issues/5518?
twoHundred :: Text
twoHundred = T.singleton '\129730'

testEncode :: Text -> Text -> Expectation
testEncode text bottom = unBottom (encode text) `shouldBe` encodeUtf8 bottom

main :: IO ()
main = hspec $ do
  -- Taken from https://github.com/bottom-software-foundation/bottom-rs/blob/need_top/src/bottom.rs
  describe "Data.Encoding.Bottom" $ do
    it "encodes strings" $ do
      testEncode "Test" "💖✨✨✨,,,,👉👈💖💖,👉👈💖💖✨🥺👉👈💖💖✨🥺,👉👈"
      testEncode "h" "💖💖,,,,👉👈"
      testEncode "🥺" $ twoHundred <> "✨✨✨✨👉👈💖💖💖🥺,,,,👉👈💖💖💖✨🥺👉👈💖💖💖✨✨✨🥺,👉👈"
      testEncode "がんばれ" $
        twoHundred
          <> "✨✨🥺,,👉👈💖💖✨✨🥺,,,,👉👈💖💖✨✨✨✨👉👈"
          <> twoHundred
          <> "✨✨🥺,,👉👈💖💖✨✨✨👉👈💖💖✨✨✨✨🥺,,👉👈"
          <> twoHundred
          <> "✨✨🥺,,👉👈💖💖✨✨🥺,,,,👉👈💖💖💖✨✨🥺,👉👈"
          <> twoHundred
          <> "✨✨🥺,,👉👈💖💖✨✨✨👉👈💖💖✨✨✨✨👉👈"
