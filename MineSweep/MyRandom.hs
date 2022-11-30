{-
Generate Random number list
sorted
-}
module MyRandom (getRList) where

import Data.List
import System.Random

seed :: Int
seed = 1234

unique :: (Ord a, Eq a) => [a] -> [a]
unique xs = remove $ sort xs
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)

getRandomList :: Int-> Int -> Int -> [Int]
-- k : unique list length
-- m : sample list length
-- n : sample range (1, n) 
getRandomList k m n 
  | length result == k = result
  | otherwise = (getRandomList k (m+k) n)
  where result = take k $ nub $ take m $ randomRs (1, n) (mkStdGen seed)

getRList :: Int -> Int -> [Int]
-- k : unique sorted list length
-- n : sample range (1, n) 
getRList 0 _ = []
getRList k n 
 | k < 0 = []
 | k < n = sort $ getRandomList k k n
 | otherwise = [1..n]


------Test------
{-
main = do 
  print $ getRList (-1) 10
  print $ getRList 0 10
  print $ getRList 1 10
  print $ getRList 7 10
  print $ getRList 10 10
  print $ getRList 11 10
-}