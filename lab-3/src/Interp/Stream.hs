module Interp.Stream
  ( initialState
  , acceptPoint
  , finalizeOutputs
  , collectOutputs
  ) where

import Control.Applicative ((<|>))
import qualified Data.List as List
import Data.Maybe (listToMaybe, maybeToList)
import Interp.Algorithms
import Interp.Types

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
