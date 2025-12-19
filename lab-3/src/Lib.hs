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
  ) where

import Control.Applicative ((<|>))
import Data.List (minimumBy)
import qualified Data.List as List
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Numeric (showFFloat)
import Text.Read (readMaybe)

data Point = Point
  { px :: Double
  , py :: Double
  }
  deriving (Show, Eq)

data Algorithm
  = Linear
  | Newton Int
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

initialState :: Config -> State
initialState cfg =
  State
    { knownPoints = []
    , algStates = map (\alg -> AlgState alg Nothing Nothing) (cfgAlgorithms cfg)
    }

acceptPoint :: Config -> State -> Point -> (State, [Output])
acceptPoint cfg state newPoint =
  produce cfg False state {knownPoints = knownPoints state ++ [newPoint]}

finalizeOutputs :: Config -> State -> [Output]
finalizeOutputs cfg state = snd (produce cfg True state)

collectOutputs :: Config -> [Point] -> [Output]
collectOutputs cfg pts =
  let (state', build) = List.foldl' step (initialState cfg, id) pts
      step (st, accF) p =
        let (st', outs) = acceptPoint cfg st p
         in (st', accF . (outs ++))
      outputs = build []
   in outputs ++ finalizeOutputs cfg state'

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
    removeTrailingZeroes s =
      let trimmed = reverse (dropWhile (== '0') (reverse s))
       in if not (null trimmed) && last trimmed == '.'
            then init trimmed
            else trimmed

produce :: Config -> Bool -> State -> (State, [Output])
produce cfg isFinal state =
  let pts = knownPoints state
      results = map (produceForAlgorithm cfg isFinal pts) (algStates state)
      newStates = map fst results
      outputs = concatMap snd results
   in (state {algStates = newStates}, outputs)

produceForAlgorithm :: Config -> Bool -> [Point] -> AlgState -> (AlgState, [Output])
produceForAlgorithm cfg isFinal pts st@(AlgState spec next lastOut) =
  case pts of
    [] -> (st, [])
    _ ->
      let startTarget = next <|> (px <$> listToMaybe pts)
          enoughPoints = length pts >= windowSize spec
       in if not enoughPoints
            then (st {nextTarget = startTarget}, [])
            else
              let (nextTarget', produced, lastProduced) = emit cfg spec pts startTarget lastOut
                  finalOut = finalizeTail cfg spec pts isFinal lastProduced produced
                  updatedLast = case finalOut of
                    Just (Output _ Point {px = x}) -> Just x
                    Nothing -> lastProduced
                  updatedNext = nextTarget'
               in (AlgState spec updatedNext updatedLast, produced ++ maybeToList finalOut)

emit :: Config -> Algorithm -> [Point] -> Maybe Double -> Maybe Double -> (Maybe Double, [Output], Maybe Double)
emit cfg spec pts startTarget initialLast =
  case startTarget of
    Nothing -> (Nothing, [], initialLast)
    Just t0 -> go t0 [] initialLast
  where
    stepSize = max (cfgStep cfg) epsilon
    maxX = maybe 0 px (lastPoint pts)
    go t acc lastSeen
      | t <= maxX + epsilon =
          case interpolate spec pts t of
            Nothing -> (Just (t + stepSize), reverse acc, lastSeen)
            Just y ->
              let out = Output spec (Point t y)
                  nextT = t + stepSize
               in go nextT (out : acc) (Just t)
      | otherwise = (Just t, reverse acc, lastSeen)

finalizeTail :: Config -> Algorithm -> [Point] -> Bool -> Maybe Double -> [Output] -> Maybe Output
finalizeTail _ _ _ False _ _ = Nothing
finalizeTail _ spec pts True lastProduced _ =
  if not (length pts >= windowSize spec)
    then Nothing
    else
      let lastX = maybe 0 snd (bounds pts)
          alreadyPrinted = maybe False (\v -> abs (v - lastX) <= epsilon) lastProduced
       in if alreadyPrinted
            then Nothing
            else Output spec . Point lastX <$> interpolate spec pts lastX

interpolate :: Algorithm -> [Point] -> Double -> Maybe Double
interpolate Linear pts x = do
  window <- windowFor 2 pts x
  case window of
    [p1, p2] -> Just (linearValue p1 p2 x)
    _ -> Nothing
interpolate (Newton n) pts x = do
  window <- windowFor n pts x
  pure (newtonValue window x)

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

linearValue :: Point -> Point -> Double -> Double
linearValue Point {px = x1, py = y1} Point {px = x2, py = y2} x =
  let t = (x - x1) / (x2 - x1)
   in y1 + t * (y2 - y1)

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
      let denominators = zipWith (\x0 xk -> xk - x0) xs (drop (offset + 1) xs)
          go (a : b : restVals) (d : ds) = ((b - a) / d) : go (b : restVals) ds
          go _ _ = []
       in go (y0 : y1 : ys) denominators
    step _ _ = []

windowSize :: Algorithm -> Int
windowSize Linear = 2
windowSize (Newton n) = max 2 n

epsilon :: Double
epsilon = 1e-9
