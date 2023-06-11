{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Multicodeblock (parse) where

import Control.Applicative (optional, (<|>))
import qualified Data.Char as Char
import Data.Functor ((<&>))
import qualified Data.Maybe as Maybe
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, anySingleBut, many, manyTill, runParser)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

codeblock :: Parser (String, Maybe String, String)
codeblock = do
  _ <- "```"
  maybeTitle <- optional ("[" *> anySingle `manyTill` "]")
  language <- anySingle `manyTill` "\n"
  code <- anySingle `manyTill` "```"
  pure (language, maybeTitle, code)

multicodeblock :: Parser [(String, Maybe String, String)]
multicodeblock = do
  _ <- "<Multicodeblock>" <* many "\n"
  manyTill (codeblock <* many "\n") "</Multicodeblock>" <* many "\n"

parser :: Parser String
parser =
  unlines
    <$> many do
      (surroundMulticodeblock . joinCodeblocks <$> multicodeblock)
        <|> anyline
  where
    anyline = manyTill (anySingleBut '\n') "\n"

    joinCodeblocks :: [(String, Maybe String, String)] -> String
    joinCodeblocks codeblocks =
      codeblocks >>= \(language, maybeTitle, block) -> do
        let languageTitle = case language of
              "purescript" -> "PureScript"
              "haskell" -> "Haskell"
              "hs" -> "Haskell"
              "typescript" -> "TypeScript"
              "ts" -> "TypeScript"
              "rust" -> "Rust"
              (c : str) -> Char.toUpper c : str
              "" -> ""
        let title = Maybe.fromMaybe languageTitle maybeTitle
        unlines
          [ "<multicodeblock-tab role=\"heading\" slot=\"tab\">" <> title <> "</multicodeblock-tab>",
            "<multicodeblock-panel role=\"region\" slot=\"panel\">",
            "```",
            "",
            "```" <> languageTitle,
            block,
            "```",
            "",
            "```{=html}",
            "</multicodeblock-panel>"
          ]

    surroundMulticodeblock :: String -> String
    surroundMulticodeblock str =
      unlines
        [ "```{=html}",
          "<multicodeblock-tabs>",
          str,
          "</multicodeblock-tabs>",
          "```"
        ]

parse :: MonadFail m => String -> m String
parse =
  runParser parser "" <&> \case
    Right x -> pure x
    Left err -> fail (errorBundlePretty err)
