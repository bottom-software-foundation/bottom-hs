{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Encoding.Bottom (decode', encode, unBottom)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Options.Applicative
  ( Parser,
    ParserInfo,
    argument,
    execParser,
    flag',
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    str,
    (<**>),
    (<|>),
  )

version :: String
version = "0.1.0"

data Command
  = Bottomify
  | Regress
  | Version
  deriving (Show)

commandParser :: Parser Command
commandParser = bottomifyParser <|> regressParser <|> versionParser
  where
    bottomifyParser = flag' Bottomify $ long "bottomify" <> short 'b' <> help "Translate text to bottom"
    regressParser = flag' Regress $ long "regress" <> short 'r' <> help "Translate bottom to human-readable text (futile)"
    versionParser = flag' Version $ long "version" <> short 'V' <> help "Prints version information"

data Options = Options
  { mode :: Command,
    text :: Text
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser = Options <$> commandParser <*> textParser
  where
    textParser = argument str (metavar "<TEXT>")

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    $ fullDesc <> progDesc "Fantastic (maybe) CLI for translating between bottom and human-readable text" <> header ("Bottom translator " <> version)

main :: IO ()
main = do
  Options {mode, text} <- execParser opts
  case mode of
    Version -> putStrLn $ "Bottom translator " <> version
    Bottomify -> putStrLn $ T.unpack $ decodeUtf8 $ unBottom $ encode text
    Regress -> case decode' $ encodeUtf8 text of
      Right decoded -> putStrLn $ T.unpack decoded
      Left err -> putStrLn err
