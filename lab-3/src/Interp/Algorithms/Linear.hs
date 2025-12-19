module Interp.Algorithms.Linear
  ( linearValue
  ) where

import Interp.Types

linearValue :: Point -> Point -> Double -> Double
linearValue Point {px = x1, py = y1} Point {px = x2, py = y2} x =
  let t = (x - x1) / (x2 - x1)
   in y1 + t * (y2 - y1)
