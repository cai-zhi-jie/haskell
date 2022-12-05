{-
MineSweep Flow Control
-}
module Flow where

import Control.Monad (when)
import Data.Char
import System.Exit (exitSuccess)
import System.Random

import Grid
import Mine
import MyRandom
import Util

-- undate all grid value
initGridValue :: [[Int]] -> [[Int]]
initGridValue grid = foldl updateGridValueById grid [0..(binNum-1)]
  where
    row = length grid
    col = length $ grid !! 0 
    binNum = row * col

isManualOperation :: String -> Bool
isManualOperation opName
  | op == "M" || op == "F" || opName == "U" = True
  | otherwise = False 
    where
      op = map toUpper opName

evalState :: [[Int]] -> [[Int]] -> IO ()
evalState vgrid sgrid= do
  -- printGridByChar $ sideBySide (transferGrid vgrid) (transferGrid sgrid)
  putStrLn "$: flag | x: bomb | *: unmined grid | 0-8: surrounding bomb count"
  putStrLn $ "remained unmined grids : " ++ show(countGrid sgrid nonVisitedId)
  putStrLn $ "total bomb/ flaged bomb : " ++ show(countGrid vgrid bombId) ++ "/" ++ show(countGrid sgrid flagId)
  dispGrid vgrid sgrid
  when (evalWin vgrid sgrid) win
  when (evalLoss vgrid sgrid) loss
  return ()

evalWin :: [[Int]] -> [[Int]] -> Bool
evalWin vgrid sgrid = (0 == length (filter remain pair)) && (0 == length (filter wrongFlag pair)) && (0 == length (filter bombNum pair))
  where
    pair = zip (concat vgrid) (concat sgrid)
    remain = \(x,y) -> (x >= 0 && y == nonVisitedId)
    wrongFlag = \(x,y) -> (x >= 0 && y == flagId)
    bombNum = \(x,y) -> (x == bombId && y == visitedId)

evalLoss :: [[Int]] -> [[Int]] -> Bool
evalLoss vgrid sgrid = (0 /= length (filter bombNum pair))
  where
    pair = zip (concat vgrid) (concat sgrid)
    bombNum = \(x,y) -> (x == bombId && y == visitedId)

win :: IO()
win = do
  putStrLn "Win!!!"
  resetState

loss :: IO()
loss = do
  putStrLn "Failed!!!"
  resetState

resetState :: IO ()
resetState = do
  putStrLn "Play again?"
  putStrLn "[y] Start Over"
  putStrLn "[n] Exit"
  opName <- getLine
  let op = map toUpper opName
  let (Just action) = lookup op dispatch_flow
  action
  return() 

exit :: IO ()
exit = do
  exitSuccess

dispatch_flow :: [(String, IO ())]
dispatch_flow =  [("Y", mineSweep), ("N", exit)] 


updateStateManual :: [[Int]] -> [[Int]] -> String -> Int -> [[Int]]
updateStateManual vgrid sgrid op id
  | op == "M" = sweep vgrid sgrid id
  | op == "F" = setFlag vgrid sgrid id
  | op == "U" = unsetFlag vgrid sgrid id
  | otherwise = sgrid
  
updateManual :: [[Int]] -> [[Int]] -> String -> IO([[Int]])
updateManual vgrid sgrid op = do
  let row = length vgrid
  let col = length $ vgrid !! 0
  putStrLn $ "input rownumber: (1 -> " ++ show (row) ++ ")" 
  y <- getInt
  putStrLn $ "input colnumber: (1 -> " ++ show (col) ++ ")"
  x <- getInt
  let id = coor2index vgrid (x - 1) (y - 1)
  return (updateStateManual vgrid sgrid op id)

updateStateAutomatic :: RandomGen g => [[Int]] -> [[Int]] -> String -> g -> [[Int]]
updateStateAutomatic vgrid sgrid op seed 
  | op == "A" = automaticMine vgrid sgrid seed
  | op == "R" = randomMine vgrid sgrid (getNonVisitedId sgrid) seed
  | op == "D" = deterministicMine vgrid sgrid
  | op == "I" = inferenceMine vgrid sgrid
  | op == "P" = probabilisticMine vgrid sgrid seed
  | otherwise = sgrid 

updateAutomatic :: [[Int]] -> [[Int]] -> String -> IO([[Int]])
updateAutomatic vgrid sgrid op = do
  seed <- newStdGen
  return (updateStateAutomatic vgrid sgrid op seed)

updateState :: [[Int]] -> [[Int]] -> IO ()
updateState vgrid sgrid = do
  let row = length vgrid
  let col = length $ vgrid !! 0
  putStrLn $ "input operation: \n[M]ine \n[F]lag \n[U]nflag \nassistance: \n[A]utomaticMine \n[D]eteministicMine \n[I]nferenceMine \n[P]robabilisticMine \n[R]andomMine \n[E]xit"
  opName <- getLine
  let op = map toUpper opName
  when (op == "E") exit
  seed <- newStdGen
  if (isManualOperation op) 
    then do
      nsgrid <- updateManual vgrid sgrid op
      evalState vgrid nsgrid
      updateState vgrid nsgrid
      return ()
    else do
      nsgrid <- updateAutomatic vgrid sgrid op
      evalState vgrid nsgrid
      updateState vgrid nsgrid
      return ()
  return ()

mineSweep :: IO ()
mineSweep = do
  -- Entrypoint to the game loop
  putStrLn "Enter the number of rows:"
  row <- getInt
  putStrLn "Enter the number of columns:"
  col <- getInt
  -- let row = 10
  -- let col = 10
  putStrLn "***** MineSweep *****"
  seed <- newStdGen
  let vgrid = initGridValue $ initGridWithRandomBomb row col bombRatio seed
  let sgrid = getBlankGrid row col
  evalState vgrid sgrid
  updateState vgrid sgrid
  return ()
