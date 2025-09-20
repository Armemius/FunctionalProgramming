import           Test.Tasty
import           Test.Tasty.HUnit
import           Prob5

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
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
