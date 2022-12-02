{-
Define Grid operation
-}

import Data.Foldable (toList)
import Data.Sequence (fromList, update)
import System.Random
import Control.Monad (replicateM, when)
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)
import Data.Time


import Util
import MyRandom

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
  | i == 0 = '*'
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
  | otherwise = 'o'

printGrid :: [[Int]] -> IO()
printGrid grid = putStrLn $  (unlines $ reverse $ addSpace $ transferGrid grid)

printGridByChar :: [[Char]] -> IO()
printGridByChar grid = putStrLn $  (unlines $ reverse $ addSpace grid)

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
initGridWithRandomBomb :: RandomGen g => Int -> Int -> g -> [[Int]]
initGridWithRandomBomb row col seed = foldl addBombById grid $ getRList bombNum binNum seed
  where 
    grid = getBlankGrid row col
    binNum = row * col
    bombNum = truncate $ toRational bombRatio * toRational binNum

-- update the grid value of surrounding bomb number by index
updateGridValueById :: [[Int]] -> Int -> [[Int]]
updateGridValueById grid id = updateElementByFunction grid countSurroundingBomb id

-- undate all grid value
initGridValue :: [[Int]] -> [[Int]]
initGridValue grid = foldl updateGridValueById grid [0..(binNum-1)]
  where
    row = length grid
    col = length $ grid !! 0 
    binNum = row * col

-- breadth first search 
-- state_grid -> value_grid -> id -> updated_state_grid
bfs :: [[Int]] -> [[Int]] -> [Int] -> [[Int]]
bfs sgrid vgrid [] = sgrid
bfs sgrid vgrid (id:ids) 
  -- for visited id, try next
  | getElementById sgrid id == visitedId = bfs sgrid vgrid ids
  -- for univsited
  -- for bomb id or value-non-0 id, mark visited and failed
  | getElementById vgrid id /= 0 = bfs (updateElement sgrid visitedId id) vgrid ids
  -- for value-eq-0 id, mark visited and search neighbor
  | otherwise = bfs (updateElement sgrid visitedId id) vgrid (ids ++ neighborId)
    where
      neighborId = getSurroundingId vgrid id

-- wrap up the bfs function
sweep :: [[Int]] -> [[Int]] -> Int -> [[Int]]
sweep vgrid sgrid id = bfs sgrid vgrid [id]

-- mark the bin with flag
setFlag :: [[Int]] -> [[Int]] -> Int -> [[Int]]
setFlag vgrid sgrid id 
  | getElementById sgrid id == nonVisitedId = updateElement sgrid flagId id
  | otherwise = sgrid

-- remove the flag
unsetFlag :: [[Int]] -> [[Int]] -> Int -> [[Int]]
unsetFlag vgrid sgrid id 
  | getElementById sgrid id == flagId = updateElement sgrid nonVisitedId id
  | otherwise = sgrid


updateGameState :: [[Int]] -> [[Int]] -> IO ()
updateGameState vgrid sgrid = do
  let row = length vgrid
  let col = length $ vgrid !! 0
  putStrLn $ "input rownumber: (1 -> " ++ show (row) ++ ")" 
  y <- getInt
  putStrLn $ "input colnumber: (1 -> " ++ show (col) ++ ")"
  x <- getInt
  putStrLn $ "input operation: [M]ine, [F]lag, [U]nflag "
  opName <- getLine
  let (Just op) = lookup opName dispatch_operation
  let nsgrid = op vgrid sgrid (coor2index vgrid (x - 1) (y - 1))
  -- let nsgrid = bfs sgrid vgrid [ coor2index vgrid (x - 1) (y - 1)]
  printGrid vgrid
  printGrid nsgrid
  dispGrid vgrid nsgrid
  when (evalWin vgrid nsgrid) win
  when (opName /= "M" && opName /= "m") $ updateGameState vgrid nsgrid
  when (bombId == (vgrid !! (y - 1) !! (x - 1))) loss
  updateGameState vgrid nsgrid
  return ()


dispatch_operation :: [(String, [[Int]] -> [[Int]] -> Int -> [[Int]])]
dispatch_operation = [("M", sweep), ("m", sweep), ("F", setFlag), ("f", setFlag), ("U", unsetFlag), ("u", unsetFlag)]

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
  putStrLn "Play again?"
  putStrLn "[y]es. Start Over"
  putStrLn "[n]o. Exit"
  ans <- getLine
  let (Just action) = lookup ans dispatch_flow
  action
  return()

loss :: IO()
loss = do
  putStrLn "Failed!!!"
  putStrLn "Play again?"
  putStrLn "[y] Start Over"
  putStrLn "[n] Exit"
  ans <- getLine
  let (Just action) = lookup ans dispatch_flow
  action
  return()

exit = do
  exitSuccess

dispatch_flow :: [(String, IO ())]
dispatch_flow =  [ ("Y", main), ("yes", main), ("y", main), ("N", exit), ("no", exit), ("n", exit) ] 


-- main = do
--   -- Entrypoint to the game loop
--   putStrLn "Enter the number of rows:"
--   row <- getInt
--   putStrLn "Enter the number of columns:"
--   col <- getInt
--   putStrLn "***** MineSweep *****"
--   seed <- newStdGen
--   let vgrid = initGridValue $ initGridWithRandomBomb row col seed
--   let sgrid = getBlankGrid row col
--   dispGrid vgrid sgrid
--   updateGameState vgrid sgrid
--   return ()

-- AI minesweeping

isVisited :: [[Int]] -> Int -> Bool
isVisited sgrid id = getElementById sgrid id == visitedId

isNonVisited :: [[Int]] -> Int -> Bool
isNonVisited sgrid id = getElementById sgrid id == nonVisitedId

isFlag :: [[Int]] -> Int -> Bool
isFlag sgrid id = getElementById sgrid id == flagId

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
    isSurroundingNotAllVisited id = (0 /= length (filter (\x -> not $ isVisited sgrid x) $ getSurroundingId sgrid id))

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
{-- %%%%%%%%%%%%%%%% RANDOM SEARCH %%%%%%%%%%%%%%%%%% --}
-- vgrid sgrid candidate newStateGrid
randomMine :: RandomGen g => [[Int]] -> [[Int]] -> [Int] -> g -> [[Int]]
randomMine vgrid sgrid list seed = sweep vgrid sgrid id
  where
    id = list !! (getR (length list) seed)

{-- %%%%%%%%%%%%%%%% DETERMINISTIC SEARCH %%%%%%%%%%%%%%%%%% --}
-- vgrid sgrid id newStateGrid
determineSurrounding :: [[Int]] -> [[Int]] -> Int -> [[Int]]
determineSurrounding vgrid sgrid id 
  -- all nonvisited bins are bombs, flag them
  | nonVisitedNum + flagNum == idNum = foldl (setFlag vgrid) sgrid (filter (isNonVisited sgrid) surroundingId)
  -- all nonvisited is not bombs, sweep them 
  | flagNum == idNum = foldl (sweep vgrid) sgrid (filter (isNonVisited sgrid) surroundingId)
  -- non-deterministic, stay unchange
  | otherwise = sgrid
    where
      surroundingId = getSurroundingId sgrid id
      idNum = getElementById vgrid id
      nonVisitedNum = length $ filter (isNonVisited sgrid) surroundingId
      flagNum = length $ filter (isFlag sgrid) surroundingId

-- vgrid sgrid nsgrid
deterministicMine :: [[Int]] -> [[Int]] -> [[Int]]
deterministicMine vgrid sgrid = foldl (determineSurrounding vgrid) sgrid (getSourceId sgrid) 

{-- %%%%%%%%%%%%%%%% INFERENCE/PROBABILISTIC SEARCH %%%%%%%%%%%%%%%%%% --}
-- 
-- vgrid sgrid id is_legal
isLegal :: [[Int]] -> [[Int]] -> Int -> Bool
isLegal vgrid sgrid id = not ((idNum < flagNum) || (idNum > flagNum + nonVisitedNum))
  where
    surroundingId = getSurroundingId sgrid id
    idNum = getElementById vgrid id
    nonVisitedNum = length $ filter (isNonVisited sgrid) surroundingId
    flagNum = length $ filter (isFlag sgrid) surroundingId

-- vgrid sgrid id_list is_legal
isLegalList :: [[Int]] -> [[Int]] -> [Int] -> Bool
isLegalList vgrid sgrid idList = foldr (&&) True (map (isLegal vgrid sgrid) idList)

-- contour_list all_possible_state
enumPossibility :: [Int] -> [[Int]]
enumPossibility [] = []
enumPossibility (x:xs) = [visitedId:line | line <- subsequence] ++ [flagId:line | line <- subsequence]
  where
    subsequence = enumPossibility xs

-- vgrid sgrid sourceId contourId contourState is_legal
isGuessLegal :: [[Int]] -> [[Int]] -> [Int] -> [Int] -> [Int] -> Bool
isGuessLegal vgrid sgrid sourceId contourId contourState = isLegalList vgrid gsgrid sourceId
  where
    guess = zip contourId contourState
    gsgrid = foldl (\ g (x, y) -> updateElement g y x) sgrid guess

-- vgrid sgrid (contourId, legalGuess)
getLegalGuess :: [[Int]] -> [[Int]] -> ([Int], [[Int]])
getLegalGuess vgrid sgrid = (contourId, legalGuess)
  where
    sourceId = getSourceId sgrid
    contourId = take 10 $ getContourId sgrid sourceId
    allGuess = enumPossibility contourId
    legalGuess = filter (isGuessLegal vgrid sgrid sourceId contourId) allGuess

-- (contourId, legalGuess) (to_be_visit_list, to_be_flag_list)
inference :: ([Int], [[Int]]) -> ([Int], [Int])
inference ([], _) = ([], [])
inference (contour, []) = ([], [])
inference (contourId, legalGuess) = (visitList, flagList)
  where
    exclusive = \ l1 l2 -> (map (\(x, y) -> if x == y then x else nonVisitedId) (zip l1 l2))
    mergeResult = foldr exclusive (legalGuess !! 0) legalGuess
    visitList = filter (\x -> x >= 0) $ map (\(x, y) -> if y == visitedId then x else -1) (zip contourId mergeResult)
    flagList = filter (\x -> x >= 0) $ map (\(x, y) -> if y == flagId then x else -1) (zip contourId mergeResult)

-- vgrid sgrid nsgrid
inferenceMine :: [[Int]] -> [[Int]] -> [[Int]]
inferenceMine vgrid sgrid = fssgrid
  where
    legalGuess = getLegalGuess vgrid sgrid
    (visitList, flagList) = inference legalGuess
    ssgrid = foldl (sweep vgrid) sgrid visitList
    fssgrid = foldl (setFlag vgrid) ssgrid flagList


listMax :: [Int] -> Int
listMax [] = 0
listMax (x:xs) 
  | x > m = x
  | otherwise = m
    where
      m = listMax xs

getMaxProbabilityPair :: [(Int, Int)] -> ([Int],Int)
getMaxProbabilityPair [] = ([],0)
getMaxProbabilityPair pair = (map (\(a,b) -> a) (filter (\(x, y) -> y == maxNum) pair), maxNum)
  where
    contourId = fst $ unzip pair
    count = snd $ unzip pair
    maxNum = listMax count

-- (contourId, legalGuess) [(id, op:visit/flag)]
calProbability :: ([Int], [[Int]]) -> [(Int,Int)]
calProbability ([], _) = [] 
calProbability (contourId, [[]]) = [] 
calProbability (contourId, legalGuess) = zip visitMaxId (getUnitList visitedId (length visitMaxId))
  -- | visitMax > flagMax = zip visitMaxId (getUnitList visitedId (length visitMaxId))
  -- | visitMax < flagMax = zip flagMaxId (getUnitList flagId (length flagMaxId))
  -- | otherwise = zip (visitMaxId++flagMaxId) ((getUnitList visitedId (length visitMaxId)) ++ (getUnitList flagId (length flagMaxId)))
  where
      len = length contourId
      count = \x -> foldl (\y z -> map (\(a, b) -> if b == x then a + 1 else a) (zip y z)) (getUnitList 0 len) legalGuess
      visitCount = count visitedId
      flagCount = count flagId
      (visitMaxId,visitMax) = getMaxProbabilityPair $ zip contourId visitCount
      (flagMaxId,flagMax) = getMaxProbabilityPair $ zip contourId flagCount

-- vgrid sgrid seed ngrid
probabilisticMine :: RandomGen g => [[Int]] -> [[Int]] -> g -> [[Int]]
probabilisticMine vgrid sgrid seed 
  | op == visitedId = sweep vgrid sgrid id
  | op == flagId = setFlag vgrid sgrid id
  | otherwise = sgrid
  where
    legalGuess = getLegalGuess vgrid sgrid
    choices = calProbability legalGuess
    (id, op) = choices !! (getR (length choices) seed)

-- vgrid sgrid seed newStateGrid
miningByAI :: RandomGen g => [[Int]] -> [[Int]] -> g -> [[Int]]
miningByAI vgrid sgrid seed 
  | length sourceList == 0 = rsgrid
  | dsgrid /= sgrid = dsgrid
  | isgrid /= sgrid = isgrid
  -- | psgrid /= sgrid = psgrid
  | otherwise = rsgrid 
    where 
      sourceList = getSourceId sgrid
      rsgrid = randomMine vgrid sgrid (getNonVisitedId sgrid) seed
      dsgrid = deterministicMine vgrid sgrid
      isgrid = inferenceMine vgrid sgrid
      -- psgrid = probabilisticMine vgrid sgrid seed


updateGameStateByAI :: [[Int]] -> [[Int]] -> IO ()
updateGameStateByAI vgrid sgrid = do
  let row = length vgrid
  let col = length $ vgrid !! 0
  putStrLn $ "input operation: [N]ext, [S]till, [E]xit "
  opName <- getLine
  -- when (opName == "S" || opName == "s") $ updateGameStateByAI vgrid sgrid
  when (opName == "E" || opName == "e") exit

  seed <- newStdGen
  let nsgrid = miningByAI vgrid sgrid seed
  -- printGridByChar $ sideBySide (transferGrid vgrid) (transferGrid nsgrid)
  putStrLn $ "total bomb/ flaged bomb : " ++ show(countTotalBomb nsgrid) ++ "/" ++ show(countFlag nsgrid)
  dispGrid vgrid nsgrid
  when (evalWin vgrid nsgrid) win
  -- when (opName /= "M" && opName /= "m") $ updateGameStateByAI vgrid nsgrid
  when (evalLoss vgrid nsgrid) loss
  updateGameStateByAI vgrid nsgrid
  return ()

isAssistedOperation :: String -> Bool
isAssistedOperation opName
  | opName == "D" || opName == "d" || opName == "I" || opName == "i" || opName == "P" || opName == "p" || opName == "R" || opName == "r" = True
  | otherwise = False 

-- main = do
--   -- Entrypoint to the game loop
--   putStrLn "Enter the number of rows:"
--   row <- getInt
--   putStrLn "Enter the number of columns:"
--   col <- getInt
--   let row = 20
--   let col = 20
--   putStrLn "***** MineSweep *****"
--   seed <- newStdGen
--   let vgrid = initGridValue $ initGridWithRandomBomb row col seed
--   let sgrid = getBlankGrid row col
--   putStrLn $ "total bomb/ flaged bomb : " ++ show(countTotalBomb sgrid) ++ "/" ++ show(countFlag sgrid)
--   dispGrid vgrid sgrid
--   updateGameStateByAI vgrid sgrid
--   return ()

assistedMine :: RandomGen g => [[Int]] -> [[Int]] -> String -> g -> [[Int]]
assistedMine vgrid sgrid opName seed 
  | opName == "R" || opName == "r" = randomMine vgrid sgrid (getNonVisitedId sgrid) seed
  | opName == "D" || opName == "d" = deterministicMine vgrid sgrid
  | opName == "I" || opName == "i" = inferenceMine vgrid sgrid
  | opName == "P" || opName == "p" = probabilisticMine vgrid sgrid seed
  | otherwise = sgrid 

updateGameStateAssisted :: [[Int]] -> [[Int]] -> IO ()
updateGameStateAssisted vgrid sgrid = do
  let row = length vgrid
  let col = length $ vgrid !! 0
  putStrLn $ "input operation: \n[M]ine \n[F]lag \n[U]nflag \nassistance: \n[D]eteministicMine \n[I]nferenceMine \n[P]robabilisticMine \n[R]andomMine \n[E]xit"
  opName <- getLine
  when (opName == "E" || opName == "e") exit
  seed <- newStdGen
  let sgrid' = assistedMine vgrid sgrid opName seed 
  if (isAssistedOperation opName)  
    then do 
      putStrLn $ "total bomb/ flaged bomb : " ++ show(countTotalBomb sgrid') ++ "/" ++ show(countFlag sgrid')
      dispGrid vgrid sgrid'
      when (evalWin vgrid sgrid') win
      when (evalLoss vgrid sgrid') loss
      updateGameStateAssisted vgrid sgrid'
    else do
      putStrLn $ "input rownumber: (1 -> " ++ show (row) ++ ")" 
      y <- getInt
      putStrLn $ "input colnumber: (1 -> " ++ show (col) ++ ")"
      x <- getInt
      let (Just op) = lookup opName dispatch_operation
      let nsgrid = op vgrid sgrid' (coor2index vgrid (x - 1) (y - 1))
      putStrLn $ "total bomb/ flaged bomb : " ++ show(countTotalBomb nsgrid) ++ "/" ++ show(countFlag nsgrid)
      dispGrid vgrid nsgrid
      when (evalWin vgrid nsgrid) win
      when (evalLoss vgrid nsgrid) loss
      updateGameStateAssisted vgrid nsgrid
      return ()

main = do
  -- Entrypoint to the game loop
  putStrLn "Enter the number of rows:"
  row <- getInt
  putStrLn "Enter the number of columns:"
  col <- getInt
  putStrLn "***** MineSweep *****"
  seed <- newStdGen
  let vgrid = initGridValue $ initGridWithRandomBomb row col seed
  let sgrid = getBlankGrid row col
  putStrLn $ "total bomb/ flaged bomb : " ++ show(countTotalBomb sgrid) ++ "/" ++ show(countFlag sgrid)
  dispGrid vgrid sgrid
  updateGameStateAssisted vgrid sgrid
  return ()

-- main = do
--   seed <- newStdGen
--   let vgrid = initGridValue $ initGridWithRandomBomb 5 5 seed
--   let sgrid = sweep vgrid (getBlankGrid 5 5) 12
--   printGrid vgrid
--   printGrid sgrid
--   dispGrid vgrid sgrid
--   print $ getSourceId sgrid
--   print $ getContourId sgrid $ getSourceId sgrid


-- main = do 
--   print $ getBlankGrid 5 5
--   let vgrid = initGridValue $ initGridWithRandomBomb 5 5
--   printGrid vgrid
--   let sgrid = bfs (getBlankGrid 5 5) vgrid [5,24]
--   let grid = maskGrid vgrid sgrid
--   printGrid grid
  -- printGrid (addBombById (getBlankGrid 5 5) $ coor2index (getBlankGrid 5 5) 1 2)
  -- dispGrid (addBombById (getBlankGrid 5 5) $ coor2index (getBlankGrid 5 5) 1 2) (getBlankGrid 5 5)
  -- dispGrid (addBombById (getBlankGrid 5 5) $ coor2index (getBlankGrid 5 5) 1 2) (getBlankGrid 5 5)
  -- printGrid $ initGridWithRandomBomb 5 5
  -- printGrid $ initGridValue $ initGridWithRandomBomb 5 5
  -- printGrid $ addBombById (getBlankGrid 5 5) $ coor2index (getBlankGrid 5 5) 1 2
  -- printGrid $ addBombById (getBlankGrid 5 5) 4
  -- print $ index2coor (getBlankGrid 5 5) 4
  -- print $ getNeighbor (getBlankGrid 5 5) 4
  -- printGrid $ addBombById (getBlankGrid 5 5) 10
  -- print $ index2coor (getBlankGrid 5 5) 10
  -- print $ getNeighbor (getBlankGrid 5 5) 10
  -- printGrid $ addBombById (getBlankGrid 5 5) 12
  -- print $ index2coor (getBlankGrid 5 5) 12
  -- print $ getNeighbor (getBlankGrid 5 5) 12
  -- print $ index2coor (getBlankGrid 5 5) 11
  -- print $ index2coor (getBlankGrid 5 5) 30
  -- print $ index2coor (getBlankGrid 5 5) (-10)
  -- print $ index2coor (getBlankGrid 5 5) 12
  -- print $ coor2index (getBlankGrid 5 5) 4 4
  -- print $ coor2index (getBlankGrid 5 5) 5 5
  -- print $ coor2index (getBlankGrid 5 5) 5 (-1)
  -- print $ coor2index (getBlankGrid 5 5) (-1) (-1)

