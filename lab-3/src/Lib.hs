module Lib
  ( Point (..)
  , Algorithm (..)
  , Config (..)
  , Output (..)
  , State
  , initialState
  , acceptPoint
  , finalizeOutputs
  , parsePoint
  , renderOutput
  , collectOutputs
  , linearValue
  , lagrangeValue
  ) where

import Interp.Algorithms (lagrangeValue, linearValue)
import Interp.Format (parsePoint, renderOutput)
import Interp.Stream (acceptPoint, collectOutputs, finalizeOutputs, initialState)
import Interp.Types
