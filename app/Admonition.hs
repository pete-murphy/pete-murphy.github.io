{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Admonition (parse) where

import Control.Applicative ((<|>))
import qualified Data.Char as Char
import Data.Functor ((<&>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingleBut, many, manyTill, runParser, try)
import Text.Megaparsec.Error (errorBundlePretty)

-- | Parses GitHub-style admonition blockquotes (`> [!TIP]` etc.) and converts
-- them to <aside data-admonition="tip"> elements. Use the data-admonition
-- attribute for CSS targeting (e.g. aside[data-admonition="tip"]).
--
-- Supported types: TIP, WARNING, INFO, NOTE, CAUTION, DANGER (case-insensitive)
type Parser = Parsec Void String

admonitionLine :: Parser (String, [String])
admonitionLine = do
  _ <- "> "
  _ <- "[!"
  admonitionType <- many (anySingleBut ']')
  _ <- "]\n"
  bodyLines <- many bodyLine
  pure (admonitionType, bodyLines)
  where
    bodyLine = do
      _ <- "> "
      content <- manyTill (anySingleBut '\n') "\n"
      pure content

parser :: Parser String
parser =
  unlines
    <$> many do
      (try (uncurry renderAdmonition <$> admonitionLine)
        <|> anyline)
  where
    anyline = manyTill (anySingleBut '\n') "\n"

    renderAdmonition :: String -> [String] -> String
    renderAdmonition rawType body =
      unlines
        [ "<aside data-admonition=\"" <> normalizeType rawType <> "\">",
          "",
          unlines body,
          "</aside>"
        ]

    normalizeType = map Char.toLower . trim

    trim = dropWhile Char.isSpace . reverse . dropWhile Char.isSpace . reverse

parse :: MonadFail m => String -> m String
parse =
  runParser parser "" <&> \case
    Right x -> pure x
    Left err -> fail (errorBundlePretty err)
