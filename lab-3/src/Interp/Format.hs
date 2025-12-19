module Interp.Format
  ( parsePoint
  , renderOutput
  ) where

import Interp.Types
import Numeric (showFFloat)
import Text.Read (readMaybe)

parsePoint :: String -> Maybe Point
parsePoint line =
  case words normalized of
    [xs, ys] -> Point <$> readMaybe xs <*> readMaybe ys
    _ -> Nothing
  where
    normalized = map replaceSep line
    replaceSep c
      | c == ';' || c == ',' || c == '\t' = ' '
      | otherwise = c

renderOutput :: Output -> String
renderOutput (Output alg Point {px = x, py = y}) =
  algorithmName alg <> ": " <> fmt x <> " " <> fmt y
  where
    fmt v = removeTrailingZeroes $ showFFloat (Just 6) v ""
    algorithmName Linear = "linear"
    algorithmName (Newton _) = "newton"
    algorithmName (Lagrange _) = "lagrange"
    removeTrailingZeroes s =
      let trimmed = reverse (dropWhile (== '0') (reverse s))
       in if not (null trimmed) && last trimmed == '.'
            then init trimmed
            else trimmed
