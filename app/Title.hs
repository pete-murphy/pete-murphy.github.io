{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Title where

import Control.Lens (worded, (%~))
import Data.Function ((&))
import Data.List.Extra (dropEnd)
import Data.Maybe (mapMaybe)

parse :: String -> (String, String)
parse fullText = do
  let title =
        fullText
          & lines
          & mapMaybe \case
            '#' : ' ' : title' -> Just title'
            _ -> Nothing
          & head
      sanitizedTitle = title & worded %~ removeSurrounding "*_`"
  (title, sanitizedTitle)

removeSurrounding :: [Char] -> String -> String
removeSurrounding chars str =
  if head str `elem` chars && head str == last str
    then removeSurrounding chars (dropEnd 1 (drop 1 str))
    else str
