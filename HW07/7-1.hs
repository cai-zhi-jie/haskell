
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- Define class R
class R r where
  mysum :: Integer -> r

-- Integer \in R
instance {-# OVERLAPPABLE #-} R Integer where
  mysum :: Integer -> Integer
  mysum x = x

-- Integer -> r \in R
instance {-# OVERLAPPABLE #-} (Integral a, R r) => R (a -> r) where
  mysum :: (Integral a, R r) => Integer -> a -> r
  mysum x y = mysum (x + toInteger y)

main :: IO()
main = do
  print $ ("1+2+3+4+5+6=" ++ show (mysum 1 2 3 4 5 6 :: Integer))
  print $ ("(-1)+(-2)+(-3)+(-4)+(-5)+(-6)=" ++ show (mysum (-1) (-2) (-3) (-4) (-5) (-6) :: Integer))
