{-/*
 * @Author: Zhijie Cai 
 * @Date: 2022-09-27 22:31:06 
 * @Problem: Solve Eight Queens 
 * @Last Modified time: 2022-09-27 22:31:35
 */-}

matrixSize = 8

queen::Int->[[(Int, Int)]]
queen 0 = [[]]
queen n = [(n, y):x | x<-queen(n-1), y<-[1..matrixSize], check x (n,y)]

check::[(Int, Int)]->(Int, Int)->Bool
check [] (x2, y2) = True
check ((x1, y1):xs) (x2, y2) = and [x1 /= x2, y1 /= y2, (x1 + y1) /= (x2 + y2), (x1 - y1) /= (x2 - y2), check xs (x2, y2)]

print2DList [] = print "END"
print2DList (x:xs) = do
  print(x)
  print2DList(xs)

main::IO()
main = do
  print ("In the " ++ show(matrixSize) ++ "queen problem, there are(is) " ++ show(length(queen matrixSize)) ++ "solution(s)")
  print ("Below show the specific solution(s) in the form of (x,y)")
  print2DList(queen matrixSize)