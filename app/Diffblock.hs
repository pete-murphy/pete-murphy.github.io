{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Diffblock (parse) where

import Control.Applicative (empty, (<|>))
import Data.List (intercalate, isPrefixOf)
import Data.Functor ((<&>))
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec
  , anySingle
  , anySingleBut
  , many
  , manyTill
  , optional
  , runParser
  , getSourcePos
  , sourceLine
  , try
  , unPos
  )
import Text.Megaparsec.Error (errorBundlePretty)

-- | Diff-aware syntax highlighting for fenced code blocks.
--
-- Markdown (GFM) has no standard way to combine language syntax
-- highlighting with diff markers: @```diff@ only colours @+@/@-@ lines and
-- drops the language, and @```haskell@ highlights the language but has no
-- notion of added/removed lines.
--
-- Pandoc / Markdown Extra DO have a standard mechanism for attaching
-- attributes to fenced code blocks -- the brace attribute block, e.g.
-- @```{.haskell .numberLines}@. We piggy-back on that: a code block whose
-- attribute block contains a @.diff@ class (plus a language class) is a
-- "diff block". Inside a diff block, lines are written unified-diff style:
--
-- > ```{.haskell .diff}
-- >   data Cmd next
-- >     = Cd String next
-- > +   | Cat String (String -> next)
-- > ```
--
-- Each line begins with a marker (@+@, @-@, or a space for context) followed
-- by a single separator space and then the code. 'parse' strips the marker
-- and separator from every line, records which lines were added/removed,
-- and rewrites the block into two pieces:
--
--   1. A raw-HTML @\<style\>@ block that colours the relevant line spans.
--   2. A normal @```{.lang #db-L}@ block, which pandoc highlights as usual.
--
-- Pandoc gives a code block with an @id@ per-line span ids of the form
-- @db-L-N@ (line @N@ within the block), so the generated CSS targets exactly
-- the added/removed lines while the rest of the block keeps its language
-- highlighting.
type Parser = Parsec Void String

-- | The diff kind of a single line.
data Kind = Context | Added | Removed
  deriving (Eq, Show)

-- | Split a diff line into its kind and its code content.
--
-- A marker char (@+@, @-@, or a space) is stripped from the front, along
-- with one separator space if present. A completely empty line is context.
-- A line that doesn't start with a marker (e.g. an omitted context marker)
-- is passed through unchanged as context.
processLine :: String -> (Kind, String)
processLine line = case line of
  ""        -> (Context, "")
  ' ' : cs -> (Context, stripSep cs)
  '+' : cs -> (Added,   stripSep cs)
  '-' : cs -> (Removed, stripSep cs)
  _         -> (Context, line)
  where
    stripSep (' ' : rest) = rest
    stripSep rest         = rest

-- | Given a fenced-code info string, if it is a diff block (a brace
-- attribute block containing a @.diff@ class and some other language class),
-- return that language. @.diff@ alone returns 'Nothing' so pandoc's native
-- @diff@ syntax highlighting handles it instead.
diffLanguage :: String -> Maybe String
diffLanguage info =
  let tokens = words (filter (\c -> c /= '{' && c /= '}') info)
      classes = [drop 1 t | t <- tokens, "." `isPrefixOf` t]
  in if "diff" `elem` classes
       then case filter (/= "diff") classes of
              []      -> Nothing
              (l : _) -> Just l
       else Nothing

-- | Parse a single diff code block, returning the rendered replacement
-- (raw-HTML @\<style\>@ + the highlighted code block). Fails (so the
-- surrounding 'try' backtracks) if the info string isn't a diff block.
diffBlock :: Parser String
diffBlock = do
  lineNum <- unPos . sourceLine <$> getSourcePos
  _ <- "```"
  info <- many (anySingleBut '\n')
  lang <- maybe empty pure (diffLanguage info)
  _ <- "\n"
  code <- anySingle `manyTill` ("```" <* optional "\n")
  pure (renderDiffBlock lineNum lang code)

-- | Render a diff block as a raw-HTML @\<style\>@ block followed by a normal
-- fenced code block carrying the block @id@ pandoc needs to derive per-line
-- span ids.
renderDiffBlock :: Int -> String -> String -> String
renderDiffBlock lineNum lang code = intercalate "\n" (open <> codeLines <> ["```"])
  where
    blockId = "db-" <> show lineNum
    info
      | null lang = "{#" <> blockId <> "}"
      | otherwise = "{." <> lang <> " #" <> blockId <> "}"

    processed = processLine <$> lines code
    codeLines = snd <$> processed

    addNums = [n | (n, (Added, _))   <- zip [1 ..] processed]
    delNums = [n | (n, (Removed, _)) <- zip [1 ..] processed]

    open =
      [ "```{=html}"
      , "<style>"
      ]
        <> addRule
        <> delRule
        <> [ "</style>"
           , "```"
           , ""
           , "```" <> info
           ]

    addRule
      | not (null addNums) =
          [ selectorList addNums
              <> " &::before { background: var(--diff-add-bg);"
              <> " box-shadow: inset 2px 0 0 var(--diff-add-accent); }"
          ]
      | otherwise = []
    delRule
      | not (null delNums) =
          [ selectorList delNums
              <> " &::before { background: var(--diff-del-bg);"
              <> " box-shadow: inset 2px 0 0 var(--diff-del-accent); }"
          ]
      | otherwise = []

    selectorList ns = intercalate ", " ["#" <> blockId <> "-" <> show n | n <- ns]

parser :: Parser String
parser =
  unlines
    <$> many do
      try diffBlock <|> anyline
  where
    anyline = manyTill (anySingleBut '\n') "\n"

parse :: MonadFail m => String -> m String
parse =
  runParser parser "" <&> \case
    Right x -> pure x
    Left err -> fail (errorBundlePretty err)
