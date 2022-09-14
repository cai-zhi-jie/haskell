{-
maxThree
return the maximum num out of three input integer
-}

maxThree::Int->Int->Int->Int

maxThree x y z
  | x > y && x > z  = x
  | y > z           = y
  | otherwise       = z


main :: IO()
main = do
  print("maxThree 2 3 6 = " ++ show(maxThree 2 3 6))
  print("maxThree 8 3 6 = " ++ show(maxThree 8 3 6))
  print("maxThree (-2) (-3) (-6) = " ++ show(maxThree (-2) (-3) (-6)))
  print("maxThree (-2) 8 (-6) = " ++ show(maxThree (-2) 8 (-6)))
