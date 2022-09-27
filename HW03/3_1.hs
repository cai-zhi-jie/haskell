{-# OPTIONS_GHC -O2 -rtsopts -with-rtsopts=-K32M #-}
{-# LANGUAGE BangPatterns #-} 

--非尾递归
--空间复杂度更大O(n)，时间复杂度为O(n) linear recursion
sum2::(Num a)=>[a]->a->a 
sum2 [] !temp=temp 
sum2 (x:xs) !temp=sum2 xs (x+temp) 

--把结果放在参数上，以实现尾递归
--尾递归实现累加
--表面上是递归，实际上代码优化后复杂度和迭代相同
--代码优化后空间复杂度为O(1)，时间复杂度为O(n) iteration
sum1::(Num a)=>[a]->a 
sum1 (x:xs)=x+sum1 xs 
sum1 []=0 

--main = print (sum2 [1..10000000] 0 ) 
main = print (sum1 [1..10000000] ) 

{-
>ghc -rtsopts -O2 3.hs
>/usr/bin/time -p ./3 +RTS -K1280M –s -RTS
-}