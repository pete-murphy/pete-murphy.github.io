{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Multicodeblock (runParser) where

import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, anySingleBut, many, manyTill)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error

type Parser = Parsec Void String

codeblock :: Parser (String, String)
codeblock = do
  _ <- "```"
  title <- manyTill anySingle "\n"
  code <- manyTill anySingle "```"
  pure (title, code)

multicodeblock :: Parser [(String, String)]
multicodeblock = do
  _ <- "<Multicodeblock>" <* many "\n"
  manyTill (codeblock <* many "\n") "</Multicodeblock>" <* many "\n"

parse :: Parser String
parse =
  unlines
    <$> many do
      (surroundMulticodeblock . joinCodeblocks <$> multicodeblock)
        <|> anyline
  where
    anyline = manyTill (anySingleBut '\n') "\n"

    joinCodeblocks :: [(String, String)] -> String
    joinCodeblocks codeblocks =
      codeblocks >>= \(title, block) -> do
        let title' = case title of
              "purescript" -> "PureScript"
              "haskell" -> "Haskell"
              "hs" -> "Haskell"
              "typescript" -> "TypeScript"
              "ts" -> "TypeScript"
              "rust" -> "Rust"
              other -> other
        unlines
          [ "<multicodeblock-tab role=\"heading\" slot=\"tab\">" <> title' <> "</multicodeblock-tab>",
            "<multicodeblock-panel role=\"region\" slot=\"panel\">",
            "```",
            "",
            "```" <> title,
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

runParser :: String -> String
runParser =
  Megaparsec.runParser parse "" <&> \case
    Right x -> x
    Left err -> error (errorBundlePretty err)
