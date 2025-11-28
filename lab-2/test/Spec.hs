{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import qualified AvlSet as Set
import qualified Avl
import Data.List (sort, nub)
import Prelude hiding (null)
import GHC.Float (ceilingDouble)

main :: IO ()
main = hspec $ do
  describe "AvlSet Unit Tests" $ do
    
    describe "Basic Operations" $ do
      it "empty set is empty" $ do
        Set.null (Set.empty :: Set.AvlSet Int) `shouldBe` True
      
      it "singleton set contains one element" $ do
        let s = Set.singleton 5 :: Set.AvlSet Int
        Set.member 5 s `shouldBe` True
        Set.size s `shouldBe` 1
      
      it "insert adds element" $ do
        let s = Set.insert 1 $ Set.insert 2 $ Set.insert 3 Set.empty :: Set.AvlSet Int
        Set.member 1 s `shouldBe` True
        Set.member 2 s `shouldBe` True
        Set.member 3 s `shouldBe` True
        Set.size s `shouldBe` 3
      
      it "insert duplicate doesn't increase size" $ do
        let s = Set.insert 1 $ Set.insert 1 Set.empty :: Set.AvlSet Int
        Set.size s `shouldBe` 1
      
      it "delete removes element" $ do
        let s = Set.delete 2 $ Set.fromList [1, 2, 3] :: Set.AvlSet Int
        Set.member 2 s `shouldBe` False
        Set.member 1 s `shouldBe` True
        Set.member 3 s `shouldBe` True
        Set.size s `shouldBe` 2
      
      it "delete non-existent element does nothing" $ do
        let s = Set.delete 4 $ Set.fromList [1, 2, 3] :: Set.AvlSet Int
        Set.size s `shouldBe` 3
    
    describe "Set Operations" $ do
      it "union combines sets" $ do
        let s1 = Set.fromList [1, 2, 3] :: Set.AvlSet Int
        let s2 = Set.fromList [3, 4, 5] :: Set.AvlSet Int
        let result = Set.union s1 s2
        Set.toList result `shouldBe` [1, 2, 3, 4, 5]
      
      it "intersection finds common elements" $ do
        let s1 = Set.fromList [1, 2, 3, 4] :: Set.AvlSet Int
        let s2 = Set.fromList [3, 4, 5, 6] :: Set.AvlSet Int
        let result = Set.intersection s1 s2
        Set.toList result `shouldBe` [3, 4]
      
      it "difference removes elements" $ do
        let s1 = Set.fromList [1, 2, 3, 4] :: Set.AvlSet Int
        let s2 = Set.fromList [3, 4] :: Set.AvlSet Int
        let result = Set.difference s1 s2
        Set.toList result `shouldBe` [1, 2]
      
      it "isSubsetOf checks subset relation" $ do
        let s1 = Set.fromList [1, 2] :: Set.AvlSet Int
        let s2 = Set.fromList [1, 2, 3, 4] :: Set.AvlSet Int
        Set.isSubsetOf s1 s2 `shouldBe` True
        Set.isSubsetOf s2 s1 `shouldBe` False
    
    describe "Higher-order Functions" $ do
      it "filter keeps matching elements" $ do
        let s = Set.fromList [1, 2, 3, 4, 5, 6] :: Set.AvlSet Int
        let result = Set.filter even s
        Set.toList result `shouldBe` [2, 4, 6]
      
      it "map transforms elements" $ do
        let s = Set.fromList [1, 2, 3] :: Set.AvlSet Int
        let result = Set.map (*2) s
        Set.toList result `shouldBe` [2, 4, 6]
      
      it "foldl accumulates left to right" $ do
        let s = Set.fromList [1, 2, 3, 4] :: Set.AvlSet Int
        Set.foldl (+) 0 s `shouldBe` 10
      
      it "foldr accumulates right to left" $ do
        let s = Set.fromList [1, 2, 3, 4] :: Set.AvlSet Int
        Set.foldr (+) 0 s `shouldBe` 10
      
      it "foldl and foldr differ for non-commutative ops" $ do
        let s = Set.fromList [1, 2, 3] :: Set.AvlSet Int
        let leftResult = Set.foldl (-) 0 s
        let rightResult = Set.foldr (-) 0 s
        leftResult `shouldNotBe` rightResult
    
    describe "Conversion" $ do
      it "fromList creates set from list" $ do
        let s = Set.fromList [3, 1, 2, 1, 3] :: Set.AvlSet Int
        Set.toList s `shouldBe` [1, 2, 3]
      
      it "toList returns sorted unique elements" $ do
        let s = Set.insert 3 $ Set.insert 1 $ Set.insert 2 Set.empty :: Set.AvlSet Int
        Set.toList s `shouldBe` [1, 2, 3]
    
    describe "Equality" $ do
      it "equal sets are equal regardless of insertion order" $ do
        let s1 = Set.fromList [1, 2, 3] :: Set.AvlSet Int
        let s2 = Set.fromList [3, 2, 1] :: Set.AvlSet Int
        Set.equals s1 s2 `shouldBe` True
        (s1 == s2) `shouldBe` True
      
      it "different sets are not equal" $ do
        let s1 = Set.fromList [1, 2, 3] :: Set.AvlSet Int
        let s2 = Set.fromList [1, 2, 4] :: Set.AvlSet Int
        Set.equals s1 s2 `shouldBe` False
        (s1 == s2) `shouldBe` False

  describe "AvlSet Property-Based Tests" $ do
    
    describe "Monoid Laws" $ do
      it "left identity: mempty <> x = x" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
          in (mempty <> s) == s
      
      it "right identity: x <> mempty = x" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
          in (s <> mempty) == s
      
      it "associativity: (x <> y) <> z = x <> (y <> z)" $ property $
        \(xs :: [Int]) (ys :: [Int]) (zs :: [Int]) ->
          let s1 = Set.fromList xs
              s2 = Set.fromList ys
              s3 = Set.fromList zs
          in ((s1 <> s2) <> s3) == (s1 <> (s2 <> s3))
    
    describe "Set Properties" $ do
      it "member after insert is True" $ property $
        \(x :: Int) (xs :: [Int]) ->
          let s = Set.insert x (Set.fromList xs)
          in Set.member x s
      
      it "member after delete is False" $ property $
        \(x :: Int) (xs :: [Int]) ->
          let s = Set.delete x (Set.fromList xs)
          in not (Set.member x s)
      
      it "size after insert increases by at most 1" $ property $
        \(x :: Int) (xs :: [Int]) ->
          let s = Set.fromList xs
              s' = Set.insert x s
          in Set.size s' <= Set.size s + 1 && Set.size s' >= Set.size s
      
      it "size after delete decreases by at most 1" $ property $
        \(x :: Int) (xs :: [Int]) ->
          let s = Set.fromList xs
              s' = Set.delete x s
          in Set.size s' >= Set.size s - 1 && Set.size s' <= Set.size s
      
      it "toList returns sorted unique elements" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
              list = Set.toList s
          in list == nub (sort xs)
      
      it "union is commutative" $ property $
        \(xs :: [Int]) (ys :: [Int]) ->
          let s1 = Set.fromList xs
              s2 = Set.fromList ys
          in Set.union s1 s2 == Set.union s2 s1
      
      it "intersection is commutative" $ property $
        \(xs :: [Int]) (ys :: [Int]) ->
          let s1 = Set.fromList xs
              s2 = Set.fromList ys
          in Set.intersection s1 s2 == Set.intersection s2 s1
      
      it "intersection is subset of both sets" $ property $
        \(xs :: [Int]) (ys :: [Int]) ->
          let s1 = Set.fromList xs
              s2 = Set.fromList ys
              inter = Set.intersection s1 s2
          in Set.isSubsetOf inter s1 && Set.isSubsetOf inter s2
    
    describe "Filter and Map Properties" $ do
      it "filter preserves subset relation" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
              filtered = Set.filter even s
          in Set.isSubsetOf filtered s
      
      it "map preserves size or reduces it" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
              mapped = Set.map (`mod` 10) s
          in Set.size mapped <= Set.size s
      
      it "filter with always True predicate returns same set" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
              filtered = Set.filter (const True) s
          in filtered == s
      
      it "filter with always False predicate returns empty set" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
              filtered = Set.filter (const False) s
          in Set.null filtered
    
    describe "Fold Properties" $ do
      it "foldl and foldr with (+) are equal" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
          in Set.foldl (+) 0 s == Set.foldr (+) 0 s
      
      it "foldl with list cons equals toList reversed" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
              folded = Set.foldl (flip (:)) [] s
          in folded == reverse (Set.toList s)
      
      it "foldr with list cons equals toList" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
              folded = Set.foldr (:) [] s
          in folded == Set.toList s
    
    describe "Equality Properties" $ do
      it "reflexivity: x == x" $ property $
        \(xs :: [Int]) ->
          let s = Set.fromList xs
          in s == s
      
      it "symmetry: if x == y then y == x" $ property $
        \(xs :: [Int]) (ys :: [Int]) ->
          let s1 = Set.fromList xs
              s2 = Set.fromList ys
          in (s1 == s2) == (s2 == s1)
      
      it "transitivity: if x == y and y == z then x == z" $ property $
        \(xs :: [Int]) ->
          let s1 = Set.fromList xs
              s2 = Set.fromList xs
              s3 = Set.fromList xs
          in (s1 == s2 && s2 == s3) ==> (s1 == s3)
      
      it "sets with same elements are equal" $ property $
        \(xs :: [Int]) ->
          let s1 = Set.fromList xs
              s2 = Set.fromList (reverse xs)
          in s1 == s2

  describe "AVL Tree Structural Properties" $ do
    describe "Balance Properties" $ do
      it "AVL tree maintains balance after insertions" $ property $
        \(xs :: [Int]) ->
          let tree = Avl.fromList xs
          in isBalanced tree
      
      it "AVL tree maintains balance after deletions" $ property $
        \(xs :: [Int]) (ys :: [Int]) ->
          let tree = Avl.fromList xs
              tree' = foldr Avl.remove tree ys
          in isBalanced tree'
      
      it "height is logarithmic" $ property $
        \(NonEmpty (xs :: [Int])) ->
          let tree = Avl.fromList xs
              n = Avl.size tree
              h = Avl.getHeight tree
              a = ceilingDouble (1.44 * logBase 2 (fromIntegral n + 2))
          in h <= a

-- Helper function to check if AVL tree is balanced
isBalanced :: Avl.AVL a -> Bool
isBalanced Avl.Empty = True
isBalanced node@(Avl.Node _ _ l r) =
  abs (Avl.balanceFactor node) <= 1 && isBalanced l && isBalanced r
