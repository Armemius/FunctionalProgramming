import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

addTwo :: Int -> Int -> Int
addTwo x y = x + y

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "2 + 3 = 5" $
      addTwo 2 3 @?= 5

  , testCase "0 + 0 = 0" $
      addTwo 0 0 @?= 0
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ QC.testProperty "addition is commutative" $
      \x y -> addTwo x y == addTwo y x

  , QC.testProperty "addition with 0 keeps number" $
      \x -> addTwo x 0 == (x :: Int)

  , QC.testProperty "addition is associative" $
      \x y z -> addTwo x (addTwo y z) == addTwo (addTwo x y) z
  ]
