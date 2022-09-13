{-
recursive sum
-}

addone::Int->Int
addone x = x + 1

subone::Int->Int
subone x = x - 1

mysum::Int->Int->Int
mysum x y
  | x == 0  = y
  | x > 0   = mysum (subone x) (addone y)
  | x < 0   = mysum (addone x) (subone y)

main = do
  print("mysum result")
  print("mysum 0 12 = " ++ show(mysum 0 12))
  print("mysum 12 0 = " ++ show(mysum 12 0))
  print("mysum 4 3 = " ++ show(mysum 4 3))
  print("mysum (-4) 3 = " ++ show(mysum (-4) 3))
  print("mysum 55 (-9) = " ++ show(mysum 55 (-9)))