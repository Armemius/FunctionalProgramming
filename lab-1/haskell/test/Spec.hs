import           Test.Tasty
import           Test.Tasty.HUnit
import           Prob5
import           Prob25

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Lab 1 tests"
  [ testGroup
    "Problem 5 variants"
    [ testCase "recProb5 10" $ recProb5 10 @?= 2520
    , testCase "tailRecProb5 10" $ tailRecProb5 10 @?= 2520
    , testCase "mapProb5 10" $ mapProb5 10 @?= 2520
    , testCase "infiniteListProb5 10" $ infiniteListProb5 10 @?= 2520
    , testCase "moduleProb5 10" $ moduleProb5 10 @?= 2520
    , testCase "recProb5 20" $ recProb5 20 @?= 232792560
    , testCase "tailRecProb5 20" $ tailRecProb5 20 @?= 232792560
    , testCase "mapProb5 20" $ mapProb5 20 @?= 232792560
    , testCase "infiniteListProb5 20" $ infiniteListProb5 20 @?= 232792560
    , testCase "moduleProb5 20" $ moduleProb5 20 @?= 232792560
    , testCase "All variants (n=10) equal"
      $ let n = 10
            vals =
              [tailRecProb5 n, mapProb5 n, infiniteListProb5 n, moduleProb5 n]
        in  all (== recProb5 n) vals @? "Mismatch between implementations"
    , testCase "All variants (n=20) equal"
      $ let n = 20
            vals =
              [tailRecProb5 n, mapProb5 n, infiniteListProb5 n, moduleProb5 n]
        in  all (== recProb5 n) vals @? "Mismatch between implementations"
    ]
  , testGroup
    "Problem 25 variants"
    [ testCase "recProb25 3" $ recProb25 3 @?= 12
    , testCase "tailRecProb25 3" $ tailRecProb25 3 @?= 12
    , testCase "mapProb25 3" $ mapProb25 3 @?= 12
    , testCase "infiniteListProb25 3" $ infiniteListProb25 3 @?= 12
    , testCase "moduleProb25 3" $ moduleProb25 3 @?= 12
    , testCase "recProb25 1000" $ recProb25 1000 @?= 4782
    , testCase "tailRecProb25 1000" $ tailRecProb25 1000 @?= 4782
    , testCase "mapProb25 1000" $ mapProb25 1000 @?= 4782
    , testCase "infiniteListProb25 1000" $ infiniteListProb25 1000 @?= 4782
    , testCase "moduleProb25 1000" $ moduleProb25 1000 @?= 4782
    , testCase "All variants (n=3) equal"
      $ let
          n = 3
          vals =
            [tailRecProb25 n, mapProb25 n, infiniteListProb25 n, moduleProb25 n]
        in
          all (== recProb25 n) vals @? "Mismatch between implementations"
    , testCase "All variants (n=1000) equal"
      $ let
          n = 1000
          vals =
            [tailRecProb25 n, mapProb25 n, infiniteListProb25 n, moduleProb25 n]
        in
          all (== recProb25 n) vals @? "Mismatch between implementations"
    ]
  ]
