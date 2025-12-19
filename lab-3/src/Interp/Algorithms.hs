module Interp.Algorithms
  ( interpolate
  , linearValue
  , newtonValue
  , lagrangeValue
  , windowFor
  , lastPoint
  , bounds
  ) where

import Data.List (minimumBy)
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Interp.Algorithms.Linear (linearValue)
import Interp.Algorithms.Lagrange (lagrangeValue)
import Interp.Algorithms.Newton (newtonValue)
import Interp.Types

interpolate :: Algorithm -> [Point] -> Double -> Maybe Double
interpolate Linear pts x = do
  window <- windowFor 2 pts x
  case window of
    [p1, p2] -> Just (linearValue p1 p2 x)
    _ -> Nothing
interpolate (Newton n) pts x = do
  window <- windowFor n pts x
  pure (newtonValue window x)
interpolate (Lagrange n) pts x = do
  window <- windowFor n pts x
  pure (lagrangeValue window x)

windowFor :: Int -> [Point] -> Double -> Maybe [Point]
windowFor k pts x
  | length pts < k = Nothing
  | otherwise =
      let windows = buildWindows k pts
          covering = filter (covers x) windows
       in if null covering
            then Nothing
            else Just (minimumBy (comparing (distanceToCenter x)) covering)
  where
    covers target w =
      case bounds w of
        Nothing -> False
        Just (a, b) -> target + epsilon >= a && target - epsilon <= b
    distanceToCenter target w =
      case bounds w of
        Nothing -> 1 / 0
        Just (a, b) -> abs (target - ((a + b) / 2))

buildWindows :: Int -> [a] -> [[a]]
buildWindows k xs = go xs (length xs)
  where
    go lst n
      | k <= 0 = []
      | n < k = []
      | otherwise =
          let win = take k lst
           in win : go (drop 1 lst) (n - 1)

lastPoint :: [Point] -> Maybe Point
lastPoint [] = Nothing
lastPoint (p : ps) = Just (List.foldl' (\_ q -> q) p ps)

bounds :: [Point] -> Maybe (Double, Double)
bounds pts = do
  firstP <- listToMaybe pts
  lastP <- lastPoint pts
  pure (px firstP, px lastP)
