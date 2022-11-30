import MyRandom

{-
main = do 
  print $ getRList (-1) 10
  print $ getRList 0 10
  print $ getRList 1 10
  print $ getRList 7 10
  print $ getRList 10 10
  print $ getRList 11 10
-}
-- cabal exec -- ghc Main.hs
import Graphics.UI.WX

main :: IO ()
main = start gui

gui :: IO ()
gui = do 
  f <- frame [text := "Hello"]
  inp <- entry f []
  out <- entry f []
  but <- button f [ text := "Hello", on command := do s <- get inp text;set out [text := "Hello" ++ s]]
  set f [ layout := floatCentre $ column 5
    [ label "What is your name?", widget inp, widget but, widget out]]