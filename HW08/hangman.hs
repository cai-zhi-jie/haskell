{-
  Hangman
-}

hangman :: IO () 
hangman = do  putStrLn "Think of a word: " 
              word <- sgetLine
              putStrLn "Try to guess it:" 
              guess word

--primitive getCh :: IO Char

sgetLine :: IO String
sgetLine = do x <- getChar
              if x == '\n' then
                do  putChar x
                    return []
              else
                do  putChar '-'
                    xs <- sgetLine
                    return (x:xs)

guess :: String -> IO () 
guess word = do putStr "> "
                xs <- getLine
                if  xs == word then 
                    putStrLn "You got it!" 
                else 
                  do  putStrLn (diff word xs) 
                      guess word

diff :: String -> String -> String 
diff xs ys = [if x ` elem ` ys then x else '-' | x <- xs]

main :: IO()
main = do hangman