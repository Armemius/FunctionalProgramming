{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified AvlSet as Set
import AvlPrinter
import Prelude hiding (null)

main :: IO ()
main = do
  putStrLn "=== AvlSet Demo ==="
  putStrLn ""
  
  putStrLn "1. Creating sets:"
  let s1 = Set.fromList [3, 1, 4, 1, 5, 9 :: Int]
  putStrLn $ "s1 = fromList [3, 1, 4, 1, 5, 9] = " ++ show (Set.toList s1)
  
  let s2 = Set.fromList [2, 7, 1, 8 :: Int]
  putStrLn $ "s2 = fromList [2, 7, 1, 8] = " ++ show (Set.toList s2)
  putStrLn ""
  
  putStrLn "2. Basic operations:"
  let s3 = Set.insert 6 s1
  putStrLn $ "insert 6 s1 = " ++ show (Set.toList s3)
  
  let s4 = Set.delete 4 s1
  putStrLn $ "delete 4 s1 = " ++ show (Set.toList s4)
  
  putStrLn $ "member 5 s1 = " ++ show (Set.member 5 s1)
  putStrLn $ "member 7 s1 = " ++ show (Set.member 7 s1)
  
  putStrLn $ "size s1 = " ++ show (Set.size s1)
  putStrLn $ "null s1 = " ++ show (Set.null s1)
  putStrLn $ "null empty = " ++ show (Set.null (Set.empty :: Set.AvlSet Int))
  putStrLn ""
  
  putStrLn "3. Set operations:"
  let unionSet = Set.union s1 s2
  putStrLn $ "union s1 s2 = " ++ show (Set.toList unionSet)
  
  let interSet = Set.intersection s1 s2
  putStrLn $ "intersection s1 s2 = " ++ show (Set.toList interSet)
  
  let diffSet = Set.difference s1 s2
  putStrLn $ "difference s1 s2 = " ++ show (Set.toList diffSet)
  
  putStrLn $ "isSubsetOf [1,3] s1 = " ++ show (Set.isSubsetOf (Set.fromList [1,3]) s1)
  putStrLn ""
  
  putStrLn "4. Higher-order functions:"
  let evens = Set.filter even s1
  putStrLn $ "filter even s1 = " ++ show (Set.toList evens)
  
  let doubled = Set.map (*2) s1
  putStrLn $ "map (*2) s1 = " ++ show (Set.toList doubled)
  
  let sumSet = Set.foldl (+) 0 s1
  putStrLn $ "foldl (+) 0 s1 = " ++ show sumSet
  
  let listFromFold = Set.foldr (:) [] s1
  putStrLn $ "foldr (:) [] s1 = " ++ show listFromFold
  putStrLn ""
  
  putStrLn "5. Monoid operations:"
  let s5 = s1 <> s2
  putStrLn $ "s1 <> s2 = " ++ show (Set.toList s5)
  
  let s6 = s1 <> mempty
  putStrLn $ "s1 <> mempty = " ++ show (Set.toList s6)
  
  putStrLn $ "s1 == (s1 <> mempty) = " ++ show (s1 == s6)
  putStrLn ""
  
  putStrLn "6. Equality:"
  let s7 = Set.fromList [9, 5, 4, 3, 1 :: Int]
  putStrLn $ "s7 = fromList [9, 5, 4, 3, 1] = " ++ show (Set.toList s7)
  putStrLn $ "s1 == s7 = " ++ show (s1 == s7)
  putStrLn $ "s1 == s2 = " ++ show (s1 == s2)
  putStrLn ""
  
  putStrLn "7. Tree visualization (s1):"
  case Set.getTree s1 of
    tree -> prettyPrint tree
  putStrLn ""
