{-# LANGUAGE ImportQualifiedPost #-}

import Diffblock qualified
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Diffblock parser"
    [ testCase "transforms a {.lang .diff} block, stripping + markers" $
        Diffblock.parse "```{.haskell .diff}\n  data Foo\n+   = Bar\n```\n"
          >>= ( @?=
                  unlines
                    [ "```{=html}",
                      "<style>",
                      "#db-1 > pre > code > span { display: block; }",
                      "#db-1-2 { background: var(--diff-add-bg); box-shadow: inset 2px 0 0 var(--diff-add-accent); }",
                      "</style>",
                      "```",
                      "",
                      "```{.haskell #db-1}",
                      "data Foo",
                      "  = Bar",
                      "```"
                    ]
              ),
      testCase "assigns block id from the source line" $
        Diffblock.parse "Before\n```{.haskell .diff}\n  data Foo\n+   = Bar\n```\nAfter\n"
          >>= ( @?=
                  unlines
                    [ "Before",
                      "```{=html}",
                      "<style>",
                      "#db-2 > pre > code > span { display: block; }",
                      "#db-2-2 { background: var(--diff-add-bg); box-shadow: inset 2px 0 0 var(--diff-add-accent); }",
                      "</style>",
                      "```",
                      "",
                      "```{.haskell #db-2}",
                      "data Foo",
                      "  = Bar",
                      "```",
                      "After"
                    ]
              ),
      testCase "colors removed (-) lines with the del palette" $
        Diffblock.parse "```{.haskell .diff}\n  data Foo\n-   = Bar\n```\n"
          >>= ( @?=
                  unlines
                    [ "```{=html}",
                      "<style>",
                      "#db-1 > pre > code > span { display: block; }",
                      "#db-1-2 { background: var(--diff-del-bg); box-shadow: inset 2px 0 0 var(--diff-del-accent); }",
                      "</style>",
                      "```",
                      "",
                      "```{.haskell #db-1}",
                      "data Foo",
                      "  = Bar",
                      "```"
                    ]
              ),
      testCase "emits no add/del rule for a context-only diff block" $
        Diffblock.parse "```{.haskell .diff}\n  data Foo\n```\n"
          >>= ( @?=
                  unlines
                    [ "```{=html}",
                      "<style>",
                      "#db-1 > pre > code > span { display: block; }",
                      "</style>",
                      "```",
                      "",
                      "```{.haskell #db-1}",
                      "data Foo",
                      "```"
                    ]
              ),
      testCase "passes through non-diff code blocks unchanged" $
        Diffblock.parse "```{.haskell}\ndata Foo\n```\n"
          >>= ( @?= "```{.haskell}\ndata Foo\n```\n" ),
      testCase "passes through {.diff}-only blocks (let pandoc's diff syntax handle them)" $
        Diffblock.parse "```{.diff}\n+ foo\n```\n"
          >>= ( @?= "```{.diff}\n+ foo\n```\n" ),
      testCase "passes through plain text unchanged" $
        Diffblock.parse "Plain text\n"
          >>= ( @?= "Plain text\n" )
    ]
