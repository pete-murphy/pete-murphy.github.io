{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Title where

import Control.Arrow (first, second)
import Control.Lens (worded, (%~))
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.List.Extra (dropEnd)

parse :: MonadFail m => String -> m (String, String, String)
parse fullText = do
  let (title, rest) =
        fullText
          & lines
          & partitionMap \case
            '#' : ' ' : title' -> Left title'
            x -> Right x
          & first \case
            [] -> ""
            t : _ -> t
          & second unlines
      sanitizedTitle = title & worded %~ removeSurrounding "*_`"
  pure (title, sanitizedTitle, rest)
  where
    partitionMap :: (a -> Either x y) -> [a] -> ([x], [y])
    partitionMap f = partitionEithers . map f

removeSurrounding :: [Char] -> String -> String
removeSurrounding chars str =
  if head str `elem` chars && head str == last str
    then removeSurrounding chars (dropEnd 1 (drop 1 str))
    else str
