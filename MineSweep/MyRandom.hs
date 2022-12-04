{-
Generate Random number list
sorted
-}
module MyRandom (getRList, getR) where

import Data.List
import System.Random

import Util


getRandomList :: RandomGen g => Int-> Int -> Int -> g -> [Int]
-- k : unique list length
-- m : sample list length
-- n : sample range (1, n) 
getRandomList k m n seed
  | length result == k = result
  | otherwise = (getRandomList k (m+k) n seed)
  where result = take k $ nub $ take m $ randomRs (0, n-1) seed

getRList :: RandomGen g => Int -> Int -> g -> [Int]
-- k : unique sorted list length
-- n : sample range (1, n) 
getRList 0 _ _ = []
getRList k n seed
 | k < 0 = []
 | k < n = sort $ getRandomList k k n seed
 | otherwise = [0..(n-1)]

getR :: RandomGen g => Int -> g -> Int
getR n seed 
  | n < 0 = -1
  | otherwise = head $ randomRs (0, n-1) seed

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