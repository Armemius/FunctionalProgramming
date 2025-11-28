{-# LANGUAGE InstanceSigs #-}
module Avl (module Avl) where

data AVL a
  = Empty
  | Node
      { height :: Int
      , value  :: a
      , left   :: AVL a
      , right  :: AVL a
      }
  deriving (Show)

-- Rotations

rotateRight :: AVL a -> AVL a
rotateRight (Node _ y (Node _ x a b) c) =
  node x a (node y b c)
rotateRight t = t

rotateLeft :: AVL a -> AVL a
rotateLeft (Node _ x a (Node _ y b c)) =
  node y (node x a b) c
rotateLeft t = t

rotateLeftRight :: AVL a -> AVL a
rotateLeftRight (Node h x l r) = rotateRight (Node h x (rotateLeft l) r)
rotateLeftRight t = t

rotateRightLeft :: AVL a -> AVL a
rotateRightLeft (Node h x l r) = rotateLeft (Node h x l (rotateRight r))
rotateRightLeft t = t

-- Helper functions

size :: AVL a -> Int
size Empty = 0
size (Node _ _ l r) = 1 + size l + size r

getHeight :: AVL a -> Int
getHeight Empty = 0
getHeight n = height n

node :: a -> AVL a -> AVL a -> AVL a
node x l r = Node (1 + max (getHeight l) (getHeight r)) x l r

leaf :: a -> AVL a
leaf x = Node 1 x Empty Empty

balanceFactor :: AVL a -> Int
balanceFactor Empty = 0
balanceFactor n  = getHeight (left n) - getHeight (right n)

balance :: AVL a -> AVL a
balance t@(Node _ x l r) =
  let hl = getHeight l
      hr = getHeight r
      bf = hl - hr
  in
  if bf > 1 then
       if balanceFactor l < 0
         then rotateLeftRight t
         else rotateRight t
  else if bf < -1 then
       if balanceFactor r > 0
         then rotateRightLeft t
         else rotateLeft t
       else
         node x l r
balance t = t

-- Insertion

insert :: Ord a => a -> AVL a -> AVL a
insert x Empty = leaf x
insert x (Node _ y l r)
  | x > y = balance $ node y l (insert x r)
  | x < y = balance $ node y (insert x l) r
  | otherwise = Node (1 + max (getHeight l) (getHeight r)) y l r

-- Deletion

findMin :: AVL a -> a
findMin Empty = error "findMin: empty tree"
findMin (Node _ x Empty _) = x
findMin (Node _ _ l _) = findMin l

removeMin :: AVL a -> AVL a
removeMin Empty = Empty
removeMin (Node _ _ Empty r) = r
removeMin (Node _ x l r) = balance $ node x (removeMin l) r

remove :: Ord a => a -> AVL a -> AVL a
remove _ Empty = Empty
remove x (Node _ y l r)
  | x > y = balance $ node y l (remove x r)
  | x < y = balance $ node y (remove x l) r
  | otherwise = case (l, r) of
      (Empty, Empty) -> Empty
      (Empty, _)     -> r
      (_, Empty)     -> l
      (_, _)         -> let successor = findMin r
                        in balance $ node successor l (removeMin r)

-- Containment check

check :: Ord a => a -> AVL a -> Bool
check _ Empty = False
check x (Node _ y l r)
  | x > y = check x r
  | x < y = check x l
  | otherwise = True

-- Filter

filterTree :: Ord a => (a -> Bool) -> AVL a -> AVL a
filterTree _ Empty = Empty
filterTree predicate tree = foldLeft insertIfPredicate Empty tree
  where
    insertIfPredicate acc x = if predicate x then insert x acc else acc

-- Map

mapTree :: Ord b => (a -> b) -> AVL a -> AVL b
mapTree _ Empty = Empty
mapTree f tree = foldLeft (\acc x -> insert (f x) acc) Empty tree

-- Folds

foldLeft :: (b -> a -> b) -> b -> AVL a -> b
foldLeft _ acc Empty = acc
foldLeft f acc (Node _ x l r) = 
  let accL = foldLeft f acc l
      accX = f accL x
  in foldLeft f accX r

foldRight :: (a -> b -> b) -> b -> AVL a -> b
foldRight _ acc Empty = acc
foldRight f acc (Node _ x l r) =
  let accR = foldRight f acc r
      accX = f x accR
  in foldRight f accX l

-- Conversion

toList :: AVL a -> [a]
toList = foldRight (:) []

fromList :: Ord a => [a] -> AVL a
fromList = foldl (flip insert) Empty

-- Semantic tree equality

equals :: Eq a => AVL a -> AVL a -> Bool
equals t1 t2 = compareIterators (makeIterator t1) (makeIterator t2)
  where
    makeIterator :: AVL a -> [a]
    makeIterator = toList
    
    compareIterators :: Eq a => [a] -> [a] -> Bool
    compareIterators [] [] = True
    compareIterators [] _  = False
    compareIterators _  [] = False
    compareIterators (x:xs) (y:ys)
      | x == y    = compareIterators xs ys
      | otherwise = False

-- Proves

instance Ord a => Semigroup (AVL a) where
  Empty <> t = t
  t <> Empty = t
  t1 <> t2 = foldLeft (flip insert) t1 t2

instance Ord a => Monoid (AVL a) where
  mempty = Empty

instance Eq a => Eq (AVL a) where
  t1 == t2 = equals t1 t2

instance Foldable AVL where
  foldr :: (a -> b -> b) -> b -> AVL a -> b
  foldr = foldRight
  foldl :: (b -> a -> b) -> b -> AVL a -> b
  foldl = foldLeft
