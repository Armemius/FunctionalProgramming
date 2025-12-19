module Interp.Types
  ( Point (..)
  , Algorithm (..)
  , Config (..)
  , Output (..)
  , AlgState (..)
  , State (..)
  , epsilon
  , windowSize
  ) where

data Point = Point
  { px :: Double
  , py :: Double
  }
  deriving (Show, Eq)

data Algorithm
  = Linear
  | Newton Int
  | Lagrange Int
  deriving (Show, Eq)

data Config = Config
  { cfgStep :: Double
  , cfgAlgorithms :: [Algorithm]
  }
  deriving (Show, Eq)

data Output = Output
  { outAlgorithm :: Algorithm
  , outPoint :: Point
  }
  deriving (Show, Eq)

data AlgState = AlgState
  { algSpec :: Algorithm
  , nextTarget :: Maybe Double
  , lastEmitted :: Maybe Double
  }
  deriving (Show, Eq)

data State = State
  { knownPoints :: [Point]
  , algStates :: [AlgState]
  }
  deriving (Show, Eq)

windowSize :: Algorithm -> Int
windowSize Linear = 2
windowSize (Newton n) = max 2 n
windowSize (Lagrange n) = max 2 n

epsilon :: Double
epsilon = 1e-9
