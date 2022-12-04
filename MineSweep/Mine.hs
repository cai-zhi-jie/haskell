{-
Mine Operation
-}
module Mine where

import System.Random

import Grid
import MyRandom
import Util

{-- %%%%%%%%%%%%%%%% MANUAL MINING %%%%%%%%%%%%%%%%%% --}

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

{-- %%%%%%%%%%%%%%%% INFERENCE SEARCH %%%%%%%%%%%%%%%%%% --}
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
enumPossibility [] = [[]]
enumPossibility (x:xs) = [visitedId:line | line <- subsequence] ++ [flagId:line | line <- subsequence]
  where
    subsequence = enumPossibility xs

-- vgrid sgrid sourceId contourId contourState is_legal
isGuessLegal :: [[Int]] -> [[Int]] -> [Int] -> [Int] -> [Int] -> Bool
isGuessLegal vgrid sgrid sourceId contourId contourState = isLegalList vgrid gsgrid sourceId
  where
    guess = zip contourId contourState
    gsgrid = foldl (\ g (x, y) -> updateElement g y x) sgrid guess


-- vgrid sgrid nsgrid
inferenceMine :: [[Int]] -> [[Int]] -> [[Int]]
inferenceMine vgrid sgrid = inferenceStepByStep vgrid sgrid sourceId contourId contourId
  where
    sourceId = getSourceId sgrid
    contourId = getContourId sgrid sourceId

inferenceStepByStep :: [[Int]] -> [[Int]] -> [Int] -> [Int] -> [Int] -> [[Int]]
inferenceStepByStep _ sgrid _ _ [] = sgrid
inferenceStepByStep vgrid sgrid sourceId contourId (id:ids) 
  | ids == [] = nsgrid
  | sgrid /= nsgrid = nsgrid
  | sgrid == nsgrid = inferenceStepByStep vgrid sgrid sourceId contourId ids'
  | otherwise = sgrid
  where
    nsgrid = inferenceOneStep vgrid sgrid sourceId contourId id
    ids' = ids
    -- ids' = filter (\x -> not $ elem x (bfsSurrounding vgrid inferenceRangeOverlap contourId [] [id])) ids

inferenceOneStep :: [[Int]] -> [[Int]] -> [Int] -> [Int] -> Int -> [[Int]]
inferenceOneStep vgrid sgrid sourceId contourId id = fssgrid
  where
    legalGuess = getLegalGuessById vgrid sgrid sourceId contourId id
    (visitList, flagList) = inference legalGuess
    ssgrid = foldl (sweep vgrid) sgrid visitList
    fssgrid = foldl (setFlag vgrid) ssgrid flagList

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


-- vgrid sgrid sourceId contourId id (contourId, legalGuess)
getLegalGuessById :: [[Int]] -> [[Int]] -> [Int] -> [Int] -> Int -> ([Int], [[Int]])
getLegalGuessById vgrid sgrid sourceId contourId id = (cid, legalGuess)
  where
    cid = bfsSurrounding vgrid inferenceRange contourId [id] [id]
    allGuess = enumPossibility cid
    legalGuess = filter (isGuessLegal vgrid sgrid sourceId cid) allGuess


-- grid list start_id result
bfsSurrounding :: [[Int]] -> Int -> [Int] -> [Int] -> [Int] -> [Int]
bfsSurrounding grid num tidList hidList [] = hidList
bfsSurrounding grid num tidList hidList (id:ids) 
  | len >= num = take num result
  | otherwise = bfsSurrounding grid num tidList result (ids++cidList')
    where 
      cidList = filter (\x -> elem x tidList) $ getSurroundingId grid id
      cidList' = filter (\x -> not $ elem x hidList) cidList
      result = (hidList ++ cidList')
      len = length result

{-- %%%%%%%%%%%%%%%% PROBABILISTIC SEARCH %%%%%%%%%%%%%%%%%% --}

getMaxProbabilityPair :: [(Int, Int)] -> ([Int],Int)
getMaxProbabilityPair [] = ([],0)
getMaxProbabilityPair pair = (map (\(a,b) -> a) (filter (\(x, y) -> y == maxNum) pair), maxNum)
  where
    contourId = fst $ unzip pair
    count = snd $ unzip pair
    maxNum = listMax count

-- vgrid sgrid (contourId, legalGuess)
getLegalGuess :: [[Int]] -> [[Int]] -> ([Int], [[Int]])
getLegalGuess vgrid sgrid = (contourId, legalGuess)
  where
    sourceId = getSourceId sgrid
    contourId = take 10 $ getContourId sgrid sourceId
    allGuess = enumPossibility contourId
    legalGuess = filter (isGuessLegal vgrid sgrid sourceId contourId) allGuess

-- (contourId, legalGuess) [(id, op:visit/flag)]
calProbability :: ([Int], [[Int]]) -> [(Int,Int)]
calProbability ([], _) = [] 
calProbability (contourId, []) = [] 
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
    (id, op) =  if choices == [] then (-1, -1) else choices !! (getR (length choices) seed)

-- automining flow
-- vgrid sgrid seed newStateGrid
automaticMine :: RandomGen g => [[Int]] -> [[Int]] -> g -> [[Int]]
automaticMine vgrid sgrid seed 
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
