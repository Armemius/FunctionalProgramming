{-# LANGUAGE InstanceSigs #-}
module AvlSet
  ( AvlSet
  , empty
  , singleton
  , insert
  , delete
  , member
  , size
  , null
  , union
  , intersection
  , difference
  , isSubsetOf
  , filter
  , map
  , foldl
  , foldr
  , toList
  , fromList
  , equals
  , getTree
  ) where

import Prelude hiding (filter, foldl, foldr, map, null)
import qualified Prelude
import qualified Avl

newtype AvlSet a = AvlSet { getTree :: Avl.AVL a }
  deriving (Show)

empty :: AvlSet a
empty = AvlSet Avl.Empty

singleton :: a -> AvlSet a
singleton x = AvlSet (Avl.leaf x)

insert :: Ord a => a -> AvlSet a -> AvlSet a
insert x (AvlSet tree) = AvlSet (Avl.insert x tree)

delete :: Ord a => a -> AvlSet a -> AvlSet a
delete x (AvlSet tree) = AvlSet (Avl.remove x tree)

member :: Ord a => a -> AvlSet a -> Bool
member x (AvlSet tree) = Avl.check x tree

size :: AvlSet a -> Int
size (AvlSet tree) = Avl.size tree

null :: AvlSet a -> Bool
null (AvlSet Avl.Empty) = True
null _ = False

union :: Ord a => AvlSet a -> AvlSet a -> AvlSet a
union (AvlSet t1) (AvlSet t2) = AvlSet (t1 <> t2)

intersection :: Ord a => AvlSet a -> AvlSet a -> AvlSet a
intersection s1 s2 = filter (`member` s2) s1

difference :: Ord a => AvlSet a -> AvlSet a -> AvlSet a
difference s1 s2 = filter (\x -> not (member x s2)) s1

isSubsetOf :: Ord a => AvlSet a -> AvlSet a -> Bool
isSubsetOf s1 s2 = Prelude.all (`member` s2) (toList s1)

filter :: Ord a => (a -> Bool) -> AvlSet a -> AvlSet a
filter predicate (AvlSet tree) = AvlSet (Avl.filterTree predicate tree)

map :: Ord b => (a -> b) -> AvlSet a -> AvlSet b
map f (AvlSet tree) = AvlSet (Avl.mapTree f tree)

foldl :: (b -> a -> b) -> b -> AvlSet a -> b
foldl f acc (AvlSet tree) = Avl.foldLeft f acc tree

foldr :: (a -> b -> b) -> b -> AvlSet a -> b
foldr f acc (AvlSet tree) = Avl.foldRight f acc tree

toList :: AvlSet a -> [a]
toList (AvlSet tree) = Avl.toList tree

fromList :: Ord a => [a] -> AvlSet a
fromList xs = AvlSet (Avl.fromList xs)

equals :: Eq a => AvlSet a -> AvlSet a -> Bool
equals (AvlSet t1) (AvlSet t2) = Avl.equals t1 t2

-- Instances

instance Ord a => Semigroup (AvlSet a) where
  (<>) :: AvlSet a -> AvlSet a -> AvlSet a
  (<>) = union

instance Ord a => Monoid (AvlSet a) where
  mempty :: AvlSet a
  mempty = empty

instance Eq a => Eq (AvlSet a) where
  (==) :: AvlSet a -> AvlSet a -> Bool
  (==) = equals

instance Foldable AvlSet where
  foldr :: (a -> b -> b) -> b -> AvlSet a -> b
  foldr f acc (AvlSet tree) = Avl.foldRight f acc tree
  
  foldl :: (b -> a -> b) -> b -> AvlSet a -> b
  foldl f acc (AvlSet tree) = Avl.foldLeft f acc tree
