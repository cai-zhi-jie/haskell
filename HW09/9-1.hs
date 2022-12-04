--------------------Test Case--------------------
--- {
--- ++x;
--- ++x;
--- --x;
--- x = x + 10;
--- x = x - 5;
--- x = x * 6;
--- x = x / 3;
--- }
--- if x = 6, exp1 will return 24
import Control.Monad.State


opNum :: (Int -> Int -> Int) -> Int -> Int -> State Int Int
opNum op x y = return (op x y)

addNum :: Int -> Int -> State Int Int
addNum = opNum (+)
subNum :: Int -> Int -> State Int Int
subNum = opNum (-)
mulNum :: Int -> Int -> State Int Int
mulNum = opNum (*)
divNum :: Int -> Int -> State Int Int
divNum = opNum div

inc :: Int -> State Int Int
inc = addNum 1
--inc n = return (n + 1)
dec :: Int -> State Int Int
dec = flip subNum 1
--dec n = return (n - 1)

exp1 :: State Int Int
exp1 = do 
  x <- get
  x <- inc x
  x <- inc x
  x <- dec x
  x <- x `addNum` 10
  x <- x `subNum` 5
  x <- x `mulNum` 6
  x `divNum` 3

--------------------Result--------------------

--- evalState :: State s a -> s -> a
--- evalState p s = fst (runState p s)

--- execState :: State s a -> s -> s
--- execState p s = snd (runState p s)

main = do 
  print $ evalState exp1 6
  print $ evalState exp1 2
  print $ evalState exp1 1
  print $ evalState exp1 4