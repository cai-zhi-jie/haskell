import Test.QuickCheck

mymax::Int->Int->Int
mymax x y
  | x >= y = x
  | x < y = y

prop_Max::Int->Int->Bool
prop_Max x y = (x <= mymax x y) && (y <= mymax x y) && ( (x == mymax x y) || (y == mymax x y))

main::IO()
main = quickCheck prop_Max
