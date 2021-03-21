{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
import Data.Encoding.Bottom (decode, encode)
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main =
  defaultMain
    [ bgroup "encoding" encodeBenches,
      bgroup "decoding" decodeBenches
    ]
  where
    -- Taken from https://github.com/bottom-software-foundation/bottom-rs/blob/need_top/bench/src/bench.rs
    examples :: [(String, Text)]
    examples =
      [ ("single", "a"),
        ("short", "abcd"),
        ( "long",
          T.pack $
            unlines
              [ "What the fuck did you just fucking say about me, you",
                "little bitch? I’ll have you know I graduated top of my class in the Navy",
                "Seals, and I’ve been involved in secret raids on Al-Quaeda, and I have over",
                "300 confirmed kills. I am trained in gorilla warfare and I’m the top sniper",
                "in the entire US armed forces. You are nothing to me but just another",
                "target. I will wipe you out with precision the likes of which has never been",
                "seen before on this Earth, mark my words. You think you can get away with",
                "saying shit to me over the Internet? Think again, fucker. As we speak I am",
                "contacting my network of spies across the USA and your IP is being traced",
                "right now so you better prepare for the storm, maggot. The storm that wipes",
                "out the pathetic little thing you call your life. You’re fucking dead, kid.",
                "I can be anywhere, anytime, and I can kill you in over seven hundred ways,",
                "and that’s just with my bare hands. Not only am I extensively trained in",
                "unarmed combat, but I have access to the entire arsenal of the United States",
                "Marine Corps and I will use it to its full extent to wipe your ass off the",
                "face of the continent, you little shit. If only you could have known what",
                "unholy retribution your little “clever” comment was about to bring down upon",
                "you, maybe you would have held your tongue. You didn’t, and now you’re",
                "paying the price, you goddamn idiot. I will shit all over you and you will",
                "drown in it. You’re fucking dead, kiddo."
              ]
        )
      ]

    makeBench :: (NFData b) => (a -> b) -> (Text -> a) -> (String, Text) -> Benchmark
    makeBench f transform (name, input) = bench name $ nf f $ transform input

    encodeBenches :: [Benchmark]
    encodeBenches = makeBench encode id <$> examples

    decodeBenches :: [Benchmark]
    decodeBenches = makeBench decode encode <$> examples
