import Data.Sequence (fromList, update)
import Data.Foldable (toList)
import System.Random (randomIO)
import Control.Monad (replicateM, when)
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)

{- Helper functions for the game logic -}
-- get rows and cols from Stdinput; rows and cols are globally available vars
genBlankBoard :: (Num p1, Num p2, Enum p1, Enum p2) => p1 -> p2 -> [[Char]]
genBlankBoard rows cols = [[ '*' | c <- [0..(cols-1)]] | r <- [0..(rows-1)]]

-- print the output of the current state of the board
printBoard :: [[Char]] -> IO ()
printBoard board = putStrLn $ "\n" ++ (unlines board)

-- update the state of the board by flipping on/off
updateBoard :: [[Char]] -> Int -> Int -> [[Char]]
updateBoard board i j = toList $ update i (toList $ update j 'o' $ fromList $ board !! i) $ fromList board

-- helper function to update the state of the board to a bomb state
-- maps a boolIndex to a relevant i and j index to update the board
updateBoardWithBomb :: [[Char]] -> Int -> [[Char]]
updateBoardWithBomb board boolIndex = toList $ update i (toList $ update j '#' $ fromList $ board !! i) $ fromList board
    where
        rows = length board
        cols = length $ board !! 0
        i = boolIndex `quot` cols
        j = boolIndex `mod` cols

-- Implement update board with numInVicinity
updateBoardWithNum :: [[Char]] -> [Bool] -> Int -> Int -> [[Char]]
updateBoardWithNum board bombState i j = toList $ update i (toList $ update j result $ fromList $ board !! i) $ fromList board
    where
        num = numInVicinity board bombState i j
        result = if (num /= 0) then (show num !! 0) else 'o'

numInVicinity :: (Foldable t, Num a1) => [t a2] -> [Bool] -> Int -> Int -> a1
numInVicinity board bombState i j =
    -- check 8 sides and add up
    let cols = length $ board !! 0
        rows = length board
        boolIndexNorth = (cols * (i+1) + j)
        a = if (inBounds rows cols (i+1) j && bombState !! boolIndexNorth) then 1 else 0
        boolIndexNorthEast = (cols * (i+1) + (j+1))
        b = if (inBounds rows cols (i+1) (j+1) && bombState !! boolIndexNorthEast) then 1 else 0
        boolIndexEast = (cols * (i) + (j+1))
        c = if (inBounds rows cols i (j+1) && bombState !! boolIndexEast) then 1 else 0
        boolIndexSouthEast = (cols * (i-1) + (j+1))
        d = if (inBounds rows cols (i-1) (j+1) && bombState !! boolIndexSouthEast) then 1 else 0
        boolIndexSouth = (cols * (i-1) + j)
        e = if (inBounds rows cols (i-1) j && bombState !! boolIndexSouth) then 1 else 0
        boolIndexSouthWest = (cols * (i-1) + (j-1))
        f = if (inBounds rows cols (i-1) (j-1) && bombState !! boolIndexSouthWest) then 1 else 0
        boolIndexWest = (cols * i + (j-1))
        g = if (inBounds rows cols i (j-1) && bombState !! boolIndexWest) then 1 else 0
        boolIndexNorthWest = (cols * (i+1) + (j-1))
        h = if (inBounds rows cols (i+1) (j-1) && bombState !! boolIndexNorthWest) then 1 else 0
    in a + b + c + d + e + f + g + h

-- this should be a recursive call to keep updating the board
failedState :: [[Char]] -> [Bool] -> [Int] -> [[Char]]
failedState board bombState [] = board
failedState board bombState (boolIndex:boolArrayRange) =
    let board' = if (bombState !! boolIndex) then updateBoardWithBomb board boolIndex else board
    in failedState board' bombState boolArrayRange

-- propagate: recursively search up, down, left, right, as inspired by floodFill algorithm
propagate :: [[Char]] -> [Bool] -> Int -> Int -> [[Char]]
propagate board bombState i j =
    -- update first, then propagate. 
    if ((not $ inBounds rows cols i j) || failedBoard !! i !! j /= '*' ) then board
    -- if ((not $ inBounds rows cols i j) || board !! i !! j /= '*' ) then board
        else
            boardNorth
            where
                rows = length board
                cols = length $ board !! 0
                failedBoard = failedState board bombState [0..(length bombState - 1)]
                board' = updateBoardWithNum board bombState i j
                boardEast = propagate board' bombState (i+1) j
                boardWest = propagate boardEast bombState (i-1) j
                boardSouth = propagate boardWest bombState i (j+1)
                boardNorth = propagate boardSouth bombState i (j-1)

inBounds :: Int -> Int -> Int -> Int -> Bool
inBounds rows cols i j
    | i < 0     = False
    | i >= rows    = False
    | j < 0     = False
    | j >= cols    = False
    | otherwise = True

-- use makeBool to find a mapping for each (row,col) -> index in this array. Bind it to a session var.
makeBool :: Int -> Int -> IO [Bool]
makeBool rows cols = replicateM (rows*cols) randomIO

-- for testing purposes: less bombs (in 4x7 grid)
exampleBool = [False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False,True,True,False,False,False,True,False,True,True,False,True,False]

{- Helper functions for IO parsing -}
getInt :: IO Int
getInt = fmap read getLine

letterByLetter :: [Char] -> IO ()
letterByLetter "" = do
    putStrLn ""
    return ()
letterByLetter (x:xs) = do
    putChar x
    threadDelay 50000
    letterByLetter xs

{- Start the main loop here -}
main = do
    -- Entrypoint to the game loop
    putStrLn "Enter the number of rows: (Try 4)"
    rows <- getInt
    putStrLn "Enter the number of columns: (Try 7)"
    cols <- getInt
    bombState <- makeBool rows cols
    -- let bombState = exampleBool
    letterByLetter "W31c0me t0 M1NESW3EP3R"
    let blankBoard = genBlankBoard rows cols
    printBoard blankBoard
    updateGameState blankBoard bombState
    return ()

updateGameState :: [[Char]] -> [Bool] -> IO ()
updateGameState board bombState = do
    let rows = length board
    let cols = length $ board !! 0
    putStrLn $ "input rownumber: (0 -> " ++ show (rows-1) ++ ")" 
    i <- getInt
    putStrLn $ "input colnumber: (0 -> " ++ show (cols-1) ++ ")"
    j <- getInt
    when (bombState !! (cols * i + j)) $ printBoard $ failedState board bombState [0..(length bombState - 1)]
    when (bombState !! (cols * i + j)) $ you_die
    let newBoard = propagate board bombState i j
    printBoard newBoard
    updateGameState newBoard bombState
    return ()

you_die = do
    putStrLn "YOU DIE!!"
    threadDelay 500000
    putStrLn "YOU DIE!!"
    threadDelay 500000
    putStrLn "YOU DIE!!!!"
    threadDelay 500000
    letterByLetter "HA-HA-HA-HAHAHAHAHAHAAAA"
    threadDelay 500000
    letterByLetter "*SNIFFS* Time passes down. The pieces of your body rots, as it lies smattered across the field ..."
    letterByLetter "Like bird droppings in Piccadilly Square."
    threadDelay 1000000
    putStrLn "Would you like to play again?"
    putStrLn "[y] Start Over"
    putStrLn "[n] Exit"
    ans <- getLine
    let (Just action) = lookup ans dispatch_start_again
    action
    return()

exit = do
    exitSuccess

dispatch_start_again :: [(String, IO ())]
dispatch_start_again =  [ ("y", main), ("n", exit) ] 