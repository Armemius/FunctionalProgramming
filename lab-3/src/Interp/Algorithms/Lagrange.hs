module Interp.Algorithms.Lagrange
  ( lagrangeValue
  ) where

import Interp.Types

lagrangeValue :: [Point] -> Double -> Double
lagrangeValue pts x = sum (map basisTerm pts)
  where
    basisTerm (Point xi yi) = yi * product [scale xi xj | Point xj _ <- pts, xj /= xi]
    scale xi xj = (x - xj) / (xi - xj)
