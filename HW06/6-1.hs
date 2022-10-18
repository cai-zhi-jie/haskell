{-
  Calculate Sqrt
-}
import Debug.Trace

debug = flip trace

-- | iterative calculate sqrt

step x y = (x / y + y) / 2.0

sqrt1 :: Double -> Double
sqrt1 x = innerloop x 1.0 0.000001
  where 
  innerloop target current precision
    | abs(target - current * current) < precision = current `debug` ("y = " ++ show current)
    | otherwise = innerloop target (step x current) precision `debug` ("y = " ++ show current)


-- | fixpoint
fixpoint :: (Double -> Double) -> Double -> Double
fixpoint func start = innerloop start (func start) 0.001 `debug` ("y = " ++ show start)
  where 
  innerloop old new precision
    | abs(new - old) < precision = new `debug` ("y = " ++ show new)
    | otherwise = innerloop new (func new) precision `debug` ("y = " ++ show new)

sqrt2 :: Double -> Double
sqrt2 x = fixpoint (step x) 1.0


-- |newton iteration method
newtonIter :: (Double -> Double) -> Double -> Double
newtonIter f = fixpoint (\x -> x - (f x)/(df x))
  where df x =  (f (x + 0.001) - f x) / 0.001

sqrt3 :: Double -> Double
sqrt3 x = newtonIter (\y -> (x - y * y)) 1.0 


main :: IO()
main = do
  print("sqrt1 2 = " ++ show (sqrt1 2))
  print("sqrt2 2 = " ++ show (sqrt2 2))
  print("sqrt3 2 = " ++ show (sqrt3 2))
  print("sqrt1 7 = " ++ show (sqrt1 7))
  print("sqrt2 7 = " ++ show (sqrt2 7))
  print("sqrt3 7 = " ++ show (sqrt3 7))

