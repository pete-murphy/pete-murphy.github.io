{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Multicodeblock where

import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, anySingleBut, many, manyTill, parseTest)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error

type Parser = Parsec Void String

sample :: String
sample =
  unlines
    [ "foo",
      "bar",
      "",
      "<Multicodeblock>",
      "",
      "```typescript",
      "<div>Hello</div>",
      "```",
      "",
      "```jsx",
      "<div>Hello</div>",
      "```",
      "",
      "</Multicodeblock>"
    ]

codeblock :: Parser (String, String)
codeblock = do
  _ <- "```"
  title <- manyTill anySingle "\n"
  code <- manyTill anySingle "```"
  pure (title, code)

codeblockSample :: String
codeblockSample =
  unlines
    [ "```typescript",
      "<div>Hello</div>",
      "```",
      ""
    ]

multicodeblock :: Parser [(String, String)]
multicodeblock = do
  _ <- "<Multicodeblock>" <* many "\n"
  manyTill (codeblock <* many "\n") "</Multicodeblock>" <* many "\n"

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

parse :: Parser String
parse =
  unlines
    <$> many do
      (surroundMulticodeblock . joinCodeblocks <$> multicodeblock)
        <|> anyline

runParser =
  Megaparsec.runParser parse "" <&> \case
    Right x -> x
    Left err -> error (errorBundlePretty err)

anyline :: Parser String
anyline = do
  manyTill (anySingleBut '\n') "\n"

-- >>> import Text.Megaparsec
-- >>> runParser x' "" sample
-- Right "foo\nbar\n\n```{=html}\n<multicodeblock-tabs>\n<multicodeblock-tab role=\"heading\" slot=\"tab\">TypeScript</multicodeblock-tab>\n<multicodeblock-panel role=\"region\" slot=\"panel\">\n```\n\n```<div>Hello</div>\n```\n\n```{=html}\n  </multicodeblock-panel>\n<multicodeblock-tab role=\"heading\" slot=\"tab\">jsx</multicodeblock-tab>\n<multicodeblock-panel role=\"region\" slot=\"panel\">\n```\n\n```<div>Hello</div>\n```\n\n```{=html}\n  </multicodeblock-panel>\n\n</multicodeblock-tabs>\n```\n\n"

foo :: IO ()
foo = do
  let p :: Parser String
      -- p = between "<<<" ">>>" (many anySingle) <* eof
      p = "<<<" *> manyTill anySingle ">>>" *> many anySingle
  parseTest p "<<<foo>>>bar"
