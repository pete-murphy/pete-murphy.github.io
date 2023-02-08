{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Multicodeblock where

import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, anySingleBut, between, eof, many, manyTill, parseTest, runParser)

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
  -- code <- between "```" "```" (many (anySingleBut '`'))
  pure (title, "```" <> code <> "```")

codeblockSample :: String
codeblockSample =
  unlines
    [ "```typescript",
      "<div>Hello</div>",
      "```",
      ""
    ]

zz :: String
zz =
  unlines
    [ "<Multicodeblock>",
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

-- >>> import Text.Megaparsec
-- >>> runParser codeblock "" codeblockSample
-- Right "```typescript\n<div>Hello</div>\n```"

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
          "typescript" -> "typescript"
          "ts" -> "typescript"
          "rust" -> "Rust"
          other -> other
    unlines
      [ "<multicodeblock-tab role=\"heading\" slot=\"tab\">" <> title' <> "</multicodeblock-tab>",
        "<multicodeblock-panel role=\"region\" slot=\"panel\">",
        "```",
        "",
        block,
        "",
        "```{=html}",
        "  </multicodeblock-panel>"
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

x' :: Parser String
x' =
  unlines
    <$> many do
      (surroundMulticodeblock . joinCodeblocks <$> multicodeblock)
        <|> anyline

anyline :: Parser String
anyline = do
  manyTill (anySingleBut '\n') "\n"

-- >>> runParser x' "" sample
-- Right "foo\nbar\n\n```{=html}\n<multicodeblock-tabs>\n<multicodeblock-tab role=\"heading\" slot=\"tab\">typescript</multicodeblock-tab>\n<multicodeblock-panel role=\"region\" slot=\"panel\">\n```\n\n```<div>Hello</div>\n```\n\n```{=html}\n  </multicodeblock-panel>\n<multicodeblock-tab role=\"heading\" slot=\"tab\">jsx</multicodeblock-tab>\n<multicodeblock-panel role=\"region\" slot=\"panel\">\n```\n\n```<div>Hello</div>\n```\n\n```{=html}\n  </multicodeblock-panel>\n\n</multicodeblock-tabs>\n```\n\n"

foo :: IO ()
foo = do
  let p :: Parser String
      -- p = between "<<<" ">>>" (many anySingle) <* eof
      p = "<<<" *> manyTill anySingle ">>>" *> many anySingle
  parseTest p "<<<foo>>>bar"
