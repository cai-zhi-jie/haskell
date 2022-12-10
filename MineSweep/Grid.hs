{-
Define Grid operation
-}
module Grid where

import Data.Foldable (toList)
import Data.Sequence (fromList, update)
import System.Random

import MyRandom
import Util

bombId :: Int
bombId = (-1)
nonVisitedId :: Int
nonVisitedId = 0
visitedId :: Int
visitedId = 1
flagId :: Int
flagId = 2
bombRatio :: Double
bombRatio = 0.16
inferenceRange :: Int
inferenceRange = 10
inferenceRangeOverlap :: Int
inferenceRangeOverlap = 6

genGridByValue :: Int -> Int -> Int -> [[Int]]
genGridByValue row col v = [[v | c <- [0..(col-1)]] | r <- [0..(row-1)]]

getBlankGrid :: Int -> Int -> [[Int]]
getBlankGrid row col = genGridByValue row col 0

transferGrid :: [[Int]] -> [[Char]]
transferGrid grid = map (map int2char) grid

addSpace :: [[Char]] -> [[Char]]
addSpace grid = map (foldl (\x y -> x ++ [' '] ++ [y]) []) grid

sideBySide :: [[Char]] -> [[Char]] -> [[Char]]
sideBySide grid1 grid2 = [l1 ++ ['|'] ++ l2 | (l1, l2) <- zip grid1 grid2]

int2char :: Int -> Char
int2char i 
  | i == -1 = 'x'
  | i == 0 = '0'
  | i == 1 = '1'
  | i == 2 = '2'
  | i == 3 = '3'
  | i == 4 = '4'
  | i == 5 = '5'
  | i == 6 = '6'
  | i == 7 = '7'
  | i == 8 = '8'
  | i == 9 = '9'
  | i == 10000 = '$'
  | otherwise = '*'

printGrid :: [[Int]] -> IO()
printGrid grid = putStrLn $  (unlines $ reverse $ addSpace $ transferGrid grid)

printGridByChar :: [[Char]] -> IO()
printGridByChar grid = putStrLn $  (unlines $ reverse $ addSpace grid)

printOriginGridByChar :: [[Char]] -> IO()
printOriginGridByChar grid = putStrLn $  (unlines $ addSpace grid)

dispGrid :: [[Int]] -> [[Int]] -> IO()
dispGrid vgrid sgrid = putStrLn $ (unlines $ reverse $ addSpace $ transferGrid $ maskGrid vgrid sgrid)

-- mask_grid -> value_grid -> id -> masked_grid
maskElementById :: [[Int]] -> [[Int]] -> Int -> [[Int]]
maskElementById sgrid vgrid id
  | state == nonVisitedId = updateElement vgrid (10) id
  | state == flagId = updateElement vgrid (10000) id
  | otherwise = vgrid
    where
      state = getElementById sgrid id

-- value_grid -> mask/state_grid -> masked_grid
maskGrid :: [[Int]] -> [[Int]] -> [[Int]]
maskGrid vgrid sgrid = foldl (maskElementById sgrid) vgrid [0..(binNum-1)]
  where
    row = length vgrid
    col = length $ vgrid !! 0 
    binNum = row * col

index2coor :: Show a => [[a]] -> Int -> (Int, Int)
index2coor grid id = (x, y)
  where
    row = length grid :: Int
    col = length $ grid !! 0
    x = max 0 $ min (col - 1) $ mod id col
    y = max 0 $ min (row - 1) $ quot id col

coor2index :: Show a => [[a]] -> Int -> Int -> Int
coor2index grid x y = ny * col + nx
  where
    row = length grid
    col = length $ grid !! 0
    nx = max 0 $ min (col - 1) x
    ny = max 0 $ min (row - 1) y

getElementById :: Show a => [[a]] -> Int -> a
getElementById grid id = grid !! y !! x
  where
    (x, y) = index2coor grid id

updateElement :: Show a => [[a]] -> a -> Int ->[[a]]
updateElement grid v id = toList $ update y (toList $ update x v $ fromList $ grid !! y) $ fromList grid
  where
    (x, y) = index2coor grid id

updateElementByFunction :: Show a => [[a]] -> ([[a]] -> Int -> a) -> Int ->[[a]]
updateElementByFunction grid f id = updateElement grid (f grid id) id 

addBombById :: [[Int]] -> Int -> [[Int]]
addBombById grid id = updateElement grid bombId id

getNeighborId :: Show a => [[a]] -> Int -> [Int]
getNeighborId grid id = filter (\x-> x /= id) $ unique [ coor2index grid (x + dx) (y + dy) | (dx,dy) <- [(-1,0),(0,-1),(1,0),(0,1)]]
  where
    (x, y) = index2coor grid id

getSurroundingId :: Show a => [[a]] -> Int -> [Int]
getSurroundingId grid id = filter (\x-> x /= id) $ unique [ coor2index grid (x + dx) (y + dy) | (dx,dy) <- [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)]]
  where
    (x, y) = index2coor grid id

countSurroundingBomb :: [[Int]] -> Int -> Int
countSurroundingBomb grid id 
  -- for bomb, stay still
  | getElementById grid id == bombId = bombId 
  -- for other, calculate surrounding bomb number
  | otherwise = length $ filter (\x -> x == bombId) $ map (\(x,y) -> (grid!!y)!!x) $ map (index2coor grid) $ getSurroundingId grid id 

-- adding bombs by the random number list
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- b = [[Int]]
-- a = Int
initGridWithRandomBomb :: RandomGen g => Int -> Int -> Double -> g -> [[Int]]
initGridWithRandomBomb row col bombratio seed = foldl addBombById grid $ getRList bombNum binNum seed
  where 
    grid = getBlankGrid row col
    binNum = row * col
    bombNum = truncate $ toRational bombratio * toRational binNum

-- update the grid value of surrounding bomb number by index
updateGridValueById :: [[Int]] -> Int -> [[Int]]
updateGridValueById grid id = updateElementByFunction grid countSurroundingBomb id

-- -- breadth first search 
-- -- state_grid -> value_grid -> id -> updated_state_grid
-- bfs :: [[Int]] -> [[Int]] -> [Int] -> [[Int]]
-- bfs sgrid vgrid [] = sgrid
-- bfs sgrid vgrid (id:ids) 
--   -- for visited id, try next
--   | getElementById sgrid id == visitedId = bfs sgrid vgrid ids
--   -- -- for flag id, try next
--   -- | getElementById sgrid id == flagId = bfs sgrid vgrid ids
--   -- for univsited
--   -- for bomb id or value-non-0 id, mark visited and failed
--   | getElementById vgrid id /= 0 = bfs (updateElement sgrid visitedId id) vgrid ids
--   -- for value-eq-0 id, mark visited and search neighbor
--   | otherwise = bfs (updateElement sgrid visitedId id) vgrid (ids ++ neighborId)
--     where
--       neighborId = getSurroundingId vgrid id

isVisited :: [[Int]] -> Int -> Bool
isVisited sgrid id = isValue sgrid visitedId id

isNonVisited :: [[Int]] -> Int -> Bool
isNonVisited sgrid id = isValue sgrid nonVisitedId id

isFlag :: [[Int]] -> Int -> Bool
isFlag sgrid id = isValue sgrid flagId id

isValue :: [[Int]] -> Int -> Int -> Bool
isValue sgrid value id = getElementById sgrid id == value

countGrid :: [[Int]] -> Int -> Int
countGrid sgrid value = length $ filter (isValue sgrid value) [0..(binNum-1)]
  where
    row = length sgrid
    col = length $ sgrid !! 0 
    binNum = row * col

countFlag :: [[Int]] -> Int
countFlag sgrid = length $ filter (isFlag sgrid) [0..(binNum-1)]
  where
    row = length sgrid
    col = length $ sgrid !! 0 
    binNum = row * col

countTotalBomb :: [[Int]] -> Int
countTotalBomb grid = truncate $ toRational bombRatio * toRational binNum
  where 
    row = length grid
    col = length $ grid !! 0 
    binNum = row * col

-- state_grid -> source_list
getSourceId :: [[Int]] -> [Int]
getSourceId sgrid = filter isSurroundingNotAllVisited $ filter (isVisited sgrid) [0..(binNum-1)]
  where
    row = length sgrid
    col = length $ sgrid !! 0 
    binNum = row * col
    -- isVisited = (\x -> (getElementById sgrid x) == visitedId)
    isSurroundingNotAllVisited id = (0 /= length (filter (\x -> isNonVisited sgrid x) $ getSurroundingId sgrid id))

getContourId :: [[Int]] -> [Int] -> [Int]
getContourId sgrid sourceList = filter (isNonVisited sgrid) $ unique $ concat $ map (getSurroundingId sgrid) sourceList 
  -- where
  --   isNonVisited = (\x -> (getElementById sgrid x) == nonVisitedId)

getNonVisitedId :: [[Int]] -> [Int]
getNonVisitedId sgrid = filter (isNonVisited sgrid) [0..(binNum-1)]
  where
    row = length sgrid
    col = length $ sgrid !! 0 
    binNum = row * col

