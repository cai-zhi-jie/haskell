--[1..5] = [1 2 3 4 5]
print([1..5])
--函数式程序设计关注于计算什么
--函数程序设计的特点：无副作用（赋值语句），强类型（编译时确定而非运行时），代码重用，模块化，没有循环只有递归，高阶函数，惰性计算
--表达式不会计算，直到需要的时候才会算
--类型首字母大写，函数首字母小写
area::Int
area = 3*4
--函数运算符具有更高的优先级
--单行注释
{-多行注释-}
--基本规则: 如果新的一行开始于前一行的右边，则新行是前一行说明（定义）的继续，否则是另一个说明（定义）的开始。
--函数名和变量名以小写字母开始
--类型名和构造符名以大写字母开始
--list相同类型
--tuple不同类型的值
--curried Function有多个参数的函数可以将其放回置的类型看作函数，->右结合
--guard
max'::(Ord a)=>a->a->a
max' a b
  | a > b = a
  |otherwise = b
--where表达式
initial::String->String->String
initial firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
--Let表达式
cylinder::(RealFloat a)=>a->a->a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea
--case表达式
head'::[a]->a
head' xs = case xs of [] -> error "empty lists!"
                    (x:_) -> x

qsort::[Int]->[Int]
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]

--关键字type、data用来定义”新”类或者给类”新“名字
type Pos = (Int, Int)
type Pair a = (a, a) -- a is type variable
data Answer = Yes | No | Unknown

--非尾递归
--空间复杂度更大O(n)，时间复杂度为O(n) linear recursion
sum::[Int]->Int
sum [] = 0 --模式匹配
sum (x:xs) = x + sum xs

--把结果放在参数上，以实现尾递归
--尾递归实现累加
--表面上是递归，实际上代码优化后复杂度和迭代相同
--代码优化后空间复杂度为O(1)，时间复杂度为O(n) iteration
sum::[Int]->Int->Int
sum [] n = n
sum (x:xs) n = sum xs (x+n)

--尾递归实现累乘法
sum::[Int]->Int->Int
sum [] n = n
sum (x:xs) n = sum xs (x*n) 

--在变量前加一个！可以使得表达式强制计算
--函数调用的优先级最高

-- ++ connect string
-- show() conver * to string
--[] is any type
--["Hello", "world"] :: [[Char]] (or [String])
--运算只是中缀函数，有两种使用方法 (+) 3 2 = 5, 3 + 2 = 5
--二元函数可以使用`(backquote)转变为中缀运算， 7 `div` 2 -> 3
maxThree :: Int -> Int -> Int -> Int
maxThree x y z
  | x >= y && x >= z = x
  | y >= z = y
  | otherwise = z
--if condition then m else n
max x y = if x >= y then x else y

writeFile :: FilePath -> String -> IO( )
writeFile "myfile" "Hello" :: IO( ) --IO()代表有副作用
{-
函数的复合
(.)::(b->c)->(a-b)->(a->c)
返回是一个函数
右结合
(.)::(b->c)->(a-b)->a->c
(.) f g x = f (g x)
-}
--列表都是通过反复使用:（cons）运算得到的，其作用是再列表前添加一个元素
--[1,2,3,4] 等价于1:( 2: (3: (4: [])))
head::[a] -> a
head (x:xs) = x
head (x:_) = x
tail::[a] -> [a]
tail (_:xs) = xs
tail [1,2,3] = [2,3]
sumsq :: Int -> Int
sumsq n = sum [i^2 | i <- [1..n]]
--length返回列表元素个数
numFactors :: Int -> Int
numFactors n = length (factors n)

factors :: Int -> [Int]
factors n = [i | i <- [2..n], n `mod` i == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], isPrime x]

getPair::Int -> [(Int, Int)]
getPair n= [(i,j)| i<-[0..n], j<-[0..n], 0<i, i<j, j<=n, isPrime (i+j) ]

secPrime::Int->Int->Int
secPrime n m = (head.tail) [x|x<-[n,m], isPrime x]

--借还书系统
type Borrower = String 
type Book = String 
type Card = (Borrower, Book)
type Database = [Card]
example :: Database
example = [ (”Alice Bush”, ”The Craft of Functional Programming”),
(”Bob Clinton”, ”Million Dollar Baby”) ]
books :: Database -> Borrower -> [Book]
books db person = [book | (borrower, book) <- db, borrower == person]
borrowers :: Database -> Book -> [Borrower]
borrowed :: Database -> Book -> Bool
numBorrowed :: Database -> Borrower -> Int
numBorrowed db person = length (books db person)
--借书和还书均需要更新数据库
makeLoan :: Database -> Borrower -> Book -> Database
makeLoan db borrower book = [(borrower, book)] ++ db
returnLoan :: Database -> Book -> Database
returnLoan db book = [(borrower, book’) | (borrower, book’) <- db, book’ /= book]

--也可以构建无穷列表？[1..]

--函数式编程禁止inplace的修改，要更新数据库就必须要产生新的数据库