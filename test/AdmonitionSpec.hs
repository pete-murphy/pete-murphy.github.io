{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Admonition qualified
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Admonition parser"
    [ testCase "parses single-line TIP" $
        Admonition.parse "> [!TIP]\n> Hello world\n"
          >>= ( @?=
                  "<aside data-admonition=\"tip\">\n\nHello world\n\n</aside>"
              ),
      testCase "parses multi-line body" $
        Admonition.parse "> [!TIP]\n> Line one\n> Line two\n"
          >>= ( @?=
                  "<aside data-admonition=\"tip\">\n\nLine one\nLine two\n\n</aside>"
              ),
      testCase "parses WARNING type" $
        Admonition.parse "> [!WARNING]\n> Beware\n"
          >>= ( @?=
                  "<aside data-admonition=\"warning\">\n\nBeware\n\n</aside>"
              ),
      testCase "parses INFO type" $
        Admonition.parse "> [!INFO]\n> For your information\n"
          >>= ( @?=
                  "<aside data-admonition=\"info\">\n\nFor your information\n\n</aside>"
              ),
      testCase "normalizes type to lowercase" $
        Admonition.parse "> [!NOTE]\n> A note\n"
          >>= ( @?=
                  "<aside data-admonition=\"note\">\n\nA note\n\n</aside>"
              ),
      testCase "parses empty body" $
        Admonition.parse "> [!TIP]\n"
          >>= ( @?=
                  "<aside data-admonition=\"tip\">\n\n\n</aside>"
              ),
      testCase "passes through regular blockquotes" $
        Admonition.parse "> Just a normal blockquote\n"
          >>= ( @?=
                  "> Just a normal blockquote\n"
              ),
      testCase "passes through non-blockquote lines" $
        Admonition.parse "Plain text\n"
          >>= ( @?=
                  "Plain text\n"
              ),
      testCase "preserves surrounding content" $
        Admonition.parse "Before\n> [!TIP]\n> Tip content\nAfter\n"
          >>= ( @?=
                  "Before\n<aside data-admonition=\"tip\">\n\nTip content\n\n</aside>\nAfter"
              ),
      testCase "parses multiple admonitions" $
        Admonition.parse "> [!TIP]\n> First\n\n> [!WARNING]\n> Second\n"
          >>= ( @?=
                  "<aside data-admonition=\"tip\">\n\nFirst\n\n</aside>\n\n<aside data-admonition=\"warning\">\n\nSecond\n\n</aside>"
              ),
      testCase "preserves inline markdown in body" $
        Admonition.parse "> [!TIP]\n> The **tl;dr** is _always_ `traverse`\n"
          >>= ( @?=
                  "<aside data-admonition=\"tip\">\n\nThe **tl;dr** is _always_ `traverse`\n\n</aside>"
              )
    ]
