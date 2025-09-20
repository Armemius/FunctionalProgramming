module Prob25 (module Prob25) where

recProb25 :: Int -> Int
recProb25 x =
  let initial = 2
      limit   = 10 ^ (x - 1)
      aux :: Integer -> Integer -> Int
      aux a b | a + b >= limit = 1
              | otherwise      = 1 + aux b (a + b)
  in  initial + aux 1 1


tailRecProb25 :: Int -> Int
tailRecProb25 x =
  let initial = 2
      limit   = 10 ^ (x - 1)
      aux :: Integer -> Integer -> Int -> Int
      aux a b acc | a + b >= limit = acc + 1
                  | otherwise      = aux b (a + b) (acc + 1)
  in  initial + aux 1 1 0


mapProb25 :: Int -> Int
mapProb25 x | x <= 1    = 1
            | otherwise = last $ map moduleProb25 [1 .. x]


infiniteListProb25 :: Int -> Int
infiniteListProb25 x = (+) 1 $ length $ takeWhile (< limit) fibs
 where
  limit = 10 ^ (x - 1)
  fibs :: [Integer]
  fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)


moduleProb25 :: Int -> Int
moduleProb25 x = fst $ foldr1 const $ filter (\(_, val) -> val > limit) $ zip
  [1 ..]
  fibs
 where
  limit :: Integer
  limit = 10 ^ (x - 1)
  fibs :: [Integer]
  fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)
