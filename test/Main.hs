module Main (main) where

import Data.Encoding.Bottom (asText, encode)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec (describe, hspec, it, shouldBe)

-- Why does this cause GHC to return an error? Related to https://gitlab.haskell.org/ghc/ghc/-/issues/5518?
twoHundred :: Text
twoHundred = T.singleton '\129730'

main :: IO ()
main = hspec $ do
  -- Taken from https://github.com/bottom-software-foundation/bottom-rs/blob/need_top/src/bottom.rs
  describe "Data.Encoding.Bottom" $ do
    it "encodes strings" $ do
      asText (encode "Test") `shouldBe` "💖✨✨✨,,,,👉👈💖💖,👉👈💖💖✨🥺👉👈💖💖✨🥺,👉👈"
      asText (encode "h") `shouldBe` "💖💖,,,,👉👈"
      asText (encode "🥺") `shouldBe` twoHundred <> "✨✨✨✨👉👈💖💖💖🥺,,,,👉👈💖💖💖✨🥺👉👈💖💖💖✨✨✨🥺,👉👈"
      asText (encode "がんばれ")
        `shouldBe` twoHundred
        <> "✨✨🥺,,👉👈💖💖✨✨🥺,,,,👉👈💖💖✨✨✨✨👉👈"
        <> twoHundred
        <> "✨✨🥺,,👉👈💖💖✨✨✨👉👈💖💖✨✨✨✨🥺,,👉👈"
        <> twoHundred
        <> "✨✨🥺,,👉👈💖💖✨✨🥺,,,,👉👈💖💖💖✨✨🥺,👉👈"
        <> twoHundred
        <> "✨✨🥺,,👉👈💖💖✨✨✨👉👈💖💖✨✨✨✨👉👈"
