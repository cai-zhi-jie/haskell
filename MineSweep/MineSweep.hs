{-
MineSweep Entrance
-}

import System.Random

import Flow
import Grid
import Mine
import MyRandom
import Util

main ::IO ()
main = do
  mineSweep
  return ()

-- main = do
--   -- Entrypoint to the game loop
--   let row = 3
--   let col = 3
--   putStrLn "***** MineSweep *****"
--   seed <- newStdGen
--   let vgrid = initGridValue $ initGridWithRandomBomb row col seed
--   let sgrid = getBlankGrid row col
--   evalState vgrid sgrid
--   let s = getLegalGuess vgrid sgrid
--   print s
--   return ()
