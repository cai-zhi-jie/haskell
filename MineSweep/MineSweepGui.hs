{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-
Gui Version
-}



import Flow
import Grid
import Mine
import MyRandom
import Util

import Control.Monad (forM_, void)
import Data.Int
import Data.GI.Base
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import System.Random

printHello :: T.Text -> IO ()
printHello s = TIO.putStrLn $ "'Hello, World' from " <> s <> "!"

printQuit :: Gtk.ApplicationWindow -> IO ()
printQuit w = do
  TIO.putStrLn "Program terminated by quit button."
  Gtk.widgetDestroy w
  return ()

transferGridGUI :: [[Int]] -> [[Char]]
transferGridGUI grid = map (map int2charGUI) grid

int2charGUI :: Int -> Char
int2charGUI i 
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
  | otherwise = ' '


getButtonState :: Gtk.Button -> IO(Int)
getButtonState button = do
  t <- Gtk.getButtonLabel button
  let s = T.unpack t
  return (getButtonStateByString s)

getButtonStateByString :: String -> Int
getButtonStateByString s
  | s == " " = nonVisitedId
  | s == "$" = flagId
  | otherwise = visitedId

decodeState :: Char -> Int
decodeState c
  | c == ' ' = 0
  | c == '$' = 2
  -- | c == 'x' = 3
  | otherwise = 1

{-%%%%%%%%%%%%%% OPERATION FOR STATEBUTTON %%%%%%%%%%%%%-}
getStateFromButton :: Gtk.Button -> IO([Int])
getStateFromButton stateButton= do
  label <- Gtk.getButtonLabel stateButton
  let state = map decodeState $ T.unpack label
  return state

updateStateForButton :: Gtk.Button -> [[Int]] -> [[Int]] -> IO()
updateStateForButton stateButton vgrid sgrid= do
  let state = concat $ transferGridGUI $ maskGrid vgrid sgrid --map show (concat sgrid)
  -- print "state to button"
  -- print state
  Gtk.setButtonLabel stateButton (T.pack(state))
  return ()
  
grid1Dto2D :: [Int] -> [[Int]] -> [[Int]]
grid1Dto2D slist vgrid = foldl (\g (v, id) -> updateElement g v id) vgrid (zip slist [0 .. (len-1)])
  where
    len = length slist

evaluation :: [[Int]] -> [[Int]] -> [[Int]]
evaluation vgrid sgrid
  | evalWin vgrid sgrid = sgrid
  | evalLoss vgrid sgrid = lsgrid
  | otherwise = sgrid
    where
      row = length vgrid
      col = length $ vgrid !! 0
      bombList = filter (\id -> isValue vgrid bombId id) [0..(row*col - 1)]
      lsgrid = foldl (\g n -> updateElement g visitedId n) sgrid bombList

isEndGame :: [[Int]] -> [[Int]] -> Bool
isEndGame vgrid sgrid
  | evalWin vgrid sgrid = True
  | evalLoss vgrid sgrid = True
  | otherwise = False

newGridByState :: Gtk.Window -> Gtk.Grid -> Gtk.Button -> [[Int]] -> [[Int]] -> IO ()
newGridByState window grid stateButton vgrid sgrid = do
  let row = length sgrid
  let col = length $ sgrid !! 0

  grid' <- new Gtk.Grid [ #columnSpacing := 5
                          , #rowSpacing := 5
                          , #rowHomogeneous := True
                          , #columnHomogeneous := True]
  #remove window grid
  #add window grid'
  
  let cgrid = transferGridGUI $ maskGrid vgrid sgrid
  -- init grid button
  forM_ [0..(row*col - 1) :: Int] $ \n -> do
    let x = mod n col
    let y = quot n col
    createAndAttachBtn (T.pack[getElementById cgrid n]) window grid' stateButton "" vgrid (int2int32 x, int2int32 y, 1, 1 ) n gridButtonCallback
  -- init function button
  let label = if (evalWin vgrid sgrid) then "YOU WIN!!!" else (if (evalLoss vgrid sgrid) then "YOU LOSS!!!" else ("Unflaged bomb: " ++ show(countGrid vgrid bombId)))

  createAndAttachBtn (T.pack("Reset")) window grid' stateButton "R" vgrid (0, int2int32(row), 2, 1 ) 0 restartCallback
  createAndAttachBtn (T.pack(label)) window grid' stateButton "R" vgrid (2, int2int32(row), 8, 1 ) 0 voidCallback

  createAndAttachBtn (T.pack("Toy")) window grid' stateButton "T" vgrid (0, int2int32(row+1), 2, 1 ) 0 (scaleCallback 5 5 0.15)
  createAndAttachBtn (T.pack("Easy")) window grid' stateButton "E" vgrid (2, int2int32(row+1), 2, 1 ) 0 (scaleCallback 10 10 0.16)
  createAndAttachBtn (T.pack("Median")) window grid' stateButton "M" vgrid (4, int2int32(row+1), 2, 1 ) 0 (scaleCallback 15 15 0.2)
  createAndAttachBtn (T.pack("Hard")) window grid' stateButton "H" vgrid (6, int2int32(row+1), 2, 1 ) 0 (scaleCallback 20 20 0.2)
  createAndAttachBtn (T.pack("Hell")) window grid' stateButton "H" vgrid (8, int2int32(row+1), 2, 1 ) 0 (scaleCallback 25 25 0.3)

  createAndAttachBtn (T.pack("Auto")) window grid' stateButton "A" vgrid (0, int2int32(row+2), 2, 1 ) 0 autoMineButtonCallback
  createAndAttachBtn (T.pack("Deter")) window grid' stateButton "D" vgrid (2, int2int32(row+2), 2, 1 ) 0 autoMineButtonCallback
  createAndAttachBtn (T.pack("Infer")) window grid' stateButton "I" vgrid (4, int2int32(row+2), 2, 1 ) 0 autoMineButtonCallback
  createAndAttachBtn (T.pack("Random")) window grid' stateButton "R" vgrid (6, int2int32(row+2), 2, 1 ) 0 autoMineButtonCallback
  createAndAttachBtn (T.pack("Proba")) window grid' stateButton "P" vgrid (8, int2int32(row+2), 2, 1 ) 0 autoMineButtonCallback
  
  #showAll window 

mineByCurrentState :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
mineByCurrentState vgrid sgrid buttonState id
  | buttonState == nonVisitedId = setFlag vgrid sgrid id
  | buttonState == flagId || buttonState == visitedId = sweep vgrid sgrid id--(unsetFlag vgrid sgrid id) id
  | otherwise = sgrid

gridButtonCallback :: Gtk.Window -> Gtk.Grid -> Gtk.Button -> String -> [[Int]] -> Int -> IO ()
gridButtonCallback window grid stateButton op vgrid id = do
  let row = length vgrid
  let col = length $ vgrid !! 0
  
  slist <- getStateFromButton stateButton
  let sgrid = grid1Dto2D slist vgrid

  if (isEndGame vgrid sgrid)
    then do -- win or loss, frozen the state
      let nsgrid = evaluation vgrid sgrid
      updateStateForButton stateButton vgrid nsgrid
      newGridByState window grid stateButton vgrid nsgrid 
      return()
    else do -- in process
      let buttonState = getElementById sgrid id
      let nsgrid = mineByCurrentState vgrid sgrid buttonState id 
      updateStateForButton stateButton vgrid nsgrid
      newGridByState window grid stateButton vgrid nsgrid 
      return()


autoMineButtonCallback :: Gtk.Window -> Gtk.Grid -> Gtk.Button -> String -> [[Int]] -> Int -> IO ()
autoMineButtonCallback window grid stateButton op vgrid id = do
  let row = length vgrid
  let col = length $ vgrid !! 0
  
  slist <- getStateFromButton stateButton
  let sgrid = grid1Dto2D slist vgrid

  if (isEndGame vgrid sgrid)
    then do -- win or loss, frozen the state
      let nsgrid = evaluation vgrid sgrid
      updateStateForButton stateButton vgrid nsgrid
      newGridByState window grid stateButton vgrid nsgrid 
      return()
    else do -- in process
      nsgrid <- updateAutomatic vgrid sgrid op
      updateStateForButton stateButton vgrid nsgrid
      newGridByState window grid stateButton vgrid nsgrid 
      return()

voidCallback :: Gtk.Window -> Gtk.Grid -> Gtk.Button -> String -> [[Int]] -> Int -> IO ()
voidCallback window grid stateButton op refgrid id = do return()

restartCallback :: Gtk.Window -> Gtk.Grid -> Gtk.Button -> String -> [[Int]] -> Int -> IO ()
restartCallback window grid stateButton op refgrid id = do
  seed <- newStdGen
  let row = length refgrid
  let col = length $ refgrid !! 0
  let vgrid = initGridValue $ initGridWithRandomBomb row col bombRatio seed
  let sgrid = getBlankGrid row col

  updateStateForButton stateButton vgrid sgrid
  newGridByState window grid stateButton vgrid sgrid

scaleCallback :: Int -> Int -> Double -> Gtk.Window -> Gtk.Grid -> Gtk.Button -> String -> [[Int]] -> Int -> IO ()
scaleCallback row col ratio window grid stateButton op refgrid id = do
  seed <- newStdGen
  let vgrid = initGridValue $ initGridWithRandomBomb row col ratio seed
  let sgrid = getBlankGrid row col
  updateStateForButton stateButton vgrid sgrid
  newGridByState window grid stateButton vgrid sgrid

createAndAttachBtn :: T.Text -> Gtk.Window -> Gtk.Grid -> Gtk.Button -> String -> [[Int]] -> (Int32, Int32, Int32, Int32) -> Int -> (Gtk.Window -> Gtk.Grid -> Gtk.Button -> String -> [[Int]] -> Int -> IO ()) -> IO ()
createAndAttachBtn lbl window grid stateButton op vgrid (col, row, cspan, rspan) id callback = do
  btn <- new Gtk.Button [#label := lbl]
  on btn #clicked (callback window grid stateButton op vgrid id) 
  #attach grid btn col row cspan rspan

int32int :: Int32 -> Int
int32int i = fromIntegral (i :: Int32) :: Int

int2int32 :: Int -> Int32
int2int32 i = fromIntegral (i :: Int) :: Int32

genGrid :: IO ()
genGrid = do
  let row = 10
  let col = 10
  seed <- newStdGen
  let vgrid = initGridValue $ initGridWithRandomBomb row col bombRatio seed
  let sgrid = getBlankGrid row col

  _ <- Gtk.init Nothing
  window <- new Gtk.Window [#title := "MineSweep"]
  _ <- on window #destroy Gtk.mainQuit
  grid <- new Gtk.Grid [ #columnSpacing := (int2int32 col)
                       , #rowSpacing := (int2int32 (row+1)) ]
  #add window grid

  stateButton <- new Gtk.Button [#label := T.pack (getUnitList ' ' (row*col))]
  
  newGridByState window grid stateButton vgrid sgrid

  #showAll window

  Gtk.main

main :: IO ()
main = do
  genGrid

