{-
  Highway Path Check System
-}

type Node = (String, Int)
type Graph = [[Int]]

-- Data base
nodeList :: [Node]
nodeList = [("A", 0),
            ("B", 1),
            ("C", 2),
            ("D", 3),
            ("E", 4),
            ("F", 5)]

graph :: Graph
graph = [[0, 0, 2, 3, 0, 1],
        [0, 0, 0, 0, 3, 0],
        [2, 0, 0, 1, 0, 0],
        [3, 0, 1, 0, 0, 0],
        [0, 3, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0]]

getCityIndex :: [Node]->String->Int
getCityIndex nodelist name 
  | result == [] = -1
  | otherwise = head result
  where result = [i | (n, i)<-nodelist, n == name]

getCityName :: [Node]->Int->String
getCityName nodelist index
  | result == []    = ""
  | otherwise       = head result
    where result    = [n | (n, i)<-nodelist, i == index]


getUnvisitNeighbor :: Graph->Int->[Int]->[Int]
getUnvisitNeighbor graph index path = [x | x <- neighbor, x `notElem` path]
  where neighbor = [i | (i,v) <- zip [0..(length (graph!!index) - 1)] (graph!!index), v > 0]

dfs :: Graph->[Int]->[Int]->Int->[Int]
dfs _ [] _ _ = []
dfs graph (x:xs) path t
  | t == -1       = []                  -- exception
  | x == -1       = []                  -- exception
  | x == t        = (x:path)            -- find target
  | next /= []    = next                -- DFS traverse to a deeper level
  | otherwise     = dfs graph xs path t -- BFS traverse to a same level
    where next    = dfs graph (getUnvisitNeighbor graph x (x:path)) (x:path) t

findPath :: Graph->String->String->[String]
findPath graph s t = reverse [getCityName nodeList x | x <- dfs graph [getCityIndex nodeList s] [] (getCityIndex nodeList t)]

output graph s t = do
  if result == [] 
  then print("The path from " ++ s ++ " to " ++ t ++ " don't exist.") 
  else print("The path from " ++ s ++ " to " ++ t ++ " is " ++ (concat result))
   where result = findPath graph s t


main :: IO()
main = do
  output graph "A" "B"
  output graph "E" "F"
  output graph "B" "E"
  output graph "C" "F"

