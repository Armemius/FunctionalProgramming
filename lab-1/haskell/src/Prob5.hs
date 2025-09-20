module Prob5 (module Prob5) where

recProb5 :: Int -> Int
recProb5 x | x <= 1    = 1
           | otherwise = lcm' x (recProb5 (x - 1))
 where
  gcd' a b | b == 0    = a
           | otherwise = gcd' b (a `mod` b)
  lcm' a b = abs (a * b) `div` gcd' a b


tailRecProb5 :: Int -> Int
tailRecProb5 x = aux x 1
 where
  aux n acc | n <= 1    = acc
            | otherwise = aux (n - 1) (lcm' n acc)
  gcd' a b | b == 0    = a
           | otherwise = gcd' b (a `mod` b)
  lcm' a b = abs (a * b) `div` gcd' a b


mapProb5 :: Int -> Int
mapProb5 x | x <= 1    = 1
           | otherwise = last $ map moduleProb5 [1 .. x]


infiniteListProb5 :: Int -> Int
infiniteListProb5 x = last $ take x (aux 1 1)
 where
  aux a acc = acc : aux (a + 1) (lcm' a acc)
  gcd' a b | b == 0    = a
           | otherwise = gcd' b (a `mod` b)
  lcm' a b = abs (a * b) `div` gcd' a b


moduleProb5 :: Int -> Int
moduleProb5 x | x <= 1    = 1
              | otherwise = foldr1 lcm' [1 .. x]
 where
  gcd' a b | b == 0    = a
           | otherwise = gcd' b (a `mod` b)
  lcm' a b = abs (a * b) `div` gcd' a b
