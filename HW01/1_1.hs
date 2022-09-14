{- My first Haskell script
FirstScript.hs
-}
-- size is define as an integer
size :: Int
size = 23 + 45
square :: Int -> Int
square n = n * n
double :: Int -> Int
double x = 2 * x
example :: Int
example = double (size - square (3-23))

main :: IO()
main = do
  print("size = 23 + 45 = " ++ show(size))
  print("example = double(size-square(3-23)) = " ++ show(example))