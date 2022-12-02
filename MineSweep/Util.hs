{-
Some Tool Function
-}
module Util (unique, getInt, getUnitList) where

import Control.Concurrent (threadDelay)
import Data.List

getInt :: IO Int
getInt = fmap read getLine


unique :: (Ord a, Eq a) => [a] -> [a]
unique xs = remove $ sort xs
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)

      
getUnitList :: Int -> Int -> [Int]
getUnitList e 0 = []
getUnitList e n = e : (getUnitList e (n-1))