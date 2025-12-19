module Interp.Algorithms.Newton
  ( newtonValue
  ) where

import qualified Data.List as List
import Data.Maybe (listToMaybe, mapMaybe)
import Interp.Types

newtonValue :: [Point] -> Double -> Double
newtonValue pts x =
  let xs = map px pts
      coeffs = dividedDifferences pts
   in snd $
        List.foldl'
          (\(prod, acc) (xi, ai) -> let acc' = acc + ai * prod in (prod * (x - xi), acc'))
          (1, 0)
          (zip xs coeffs)

dividedDifferences :: [Point] -> [Double]
dividedDifferences pts = mapMaybe listToMaybe (build (map py pts) 0)
  where
    xs = map px pts
    build row offset =
      row :
        case row of
          _ : _ -> build (step row offset) (offset + 1)
          _ -> []
    step (y0 : y1 : ys) offset =
      let denominators = zipWith (flip (-)) xs (drop (offset + 1) xs)
          go (a : b : restVals) (d : ds) = ((b - a) / d) : go (b : restVals) ds
          go _ _ = []
       in go (y0 : y1 : ys) denominators
    step _ _ = []
