{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "parsePoint" $ do
    it "parses semicolon separated pairs" $ do
      parsePoint "1.5;2.0" `shouldBe` Just (Point 1.5 2.0)

    it "parses whitespace separated pairs" $ do
      parsePoint "3 4" `shouldBe` Just (Point 3 4)

    it "rejects invalid input" $ do
      parsePoint "oops" `shouldBe` Nothing

  describe "linear interpolation" $ do
    it "interpolates a simple line with step 0.5" $ do
      let cfg = Config {cfgStep = 0.5, cfgAlgorithms = [Linear]}
          pts = [Point 0 0, Point 1 1, Point 2 2]
          outputs = collectOutputs cfg pts
          values = map outPoint outputs
      values `shouldBe` [Point 0 0, Point 0.5 0.5, Point 1 1, Point 1.5 1.5, Point 2 2]

  describe "Newton interpolation" $ do
    it "matches x^2 on four points with window 3" $ do
      let cfg = Config {cfgStep = 1, cfgAlgorithms = [Newton 3]}
          pts = [Point 0 0, Point 1 1, Point 2 4, Point 3 9]
          values = map outPoint (collectOutputs cfg pts)
      values `shouldBe` [Point 0 0, Point 1 1, Point 2 4, Point 3 9]

  describe "streaming behaviour" $ do
    it "delays output until enough points arrive" $ do
      let cfg = Config {cfgStep = 1, cfgAlgorithms = [Linear]}
          st0 = initialState cfg
          (st1, out1) = acceptPoint cfg st0 (Point 0 0)
          (st2, out2) = acceptPoint cfg st1 (Point 1 1)
      out1 `shouldBe` []
      map outPoint out2 `shouldBe` [Point 0 0, Point 1 1]
      let finalOutputs = finalizeOutputs cfg st2
      map outPoint finalOutputs `shouldBe` []

  describe "properties" $ do
    it "preserves endpoints for linear interpolation" $
      property $ do
        (p1, p2) <- genDistinctPoints
        let yAtStart = linearValue p1 p2 (px p1)
            yAtEnd = linearValue p1 p2 (px p2)
        pure $ approx (py p1) yAtStart .&&. approx (py p2) yAtEnd

    it "is linear in the middle" $
      property $ do
        (p1, p2) <- genDistinctPoints
        let midX = (px p1 + px p2) / 2
            expected = (py p1 + py p2) / 2
        pure $ approx expected (linearValue p1 p2 midX)

genDistinctPoints :: Gen (Point, Point)
genDistinctPoints = do
  x1 <- finiteDouble
  delta <- choose (0.5, 20)
  y1 <- finiteDouble
  y2 <- finiteDouble
  let x2 = x1 + delta
  pure (Point x1 y1, Point x2 y2)

finiteDouble :: Gen Double
finiteDouble = choose (-1000, 1000)

approx :: Double -> Double -> Property
approx a b = counterexample ("expected " <> show a <> ", got " <> show b) (abs (a - b) < 1e-6)
