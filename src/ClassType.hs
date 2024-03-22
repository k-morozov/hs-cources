module ClassType where

data Tree a = Leaf 
            | Node (Tree a) a (Tree a) 
  deriving Show

t1 = Node (Node (Node Leaf 5 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)
t2 = Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)
t3 = Node Leaf 42 Leaf
tInf1 n = Node (tInf1 (n+2)) n (Node Leaf 42 Leaf)
tInf2 n = Node (tInf2 (n+2)) n (tInf2 (3*n-1))

elemTree :: Eq a => a -> Tree a -> Bool
elemTree number node = searchBFS number [node] 
    where
        searchBFS :: Eq a => a -> [Tree a] -> Bool
        searchBFS _ [] = False
        searchBFS target (Leaf:as) = searchBFS target as
        searchBFS target ((Node l node_value r):as)
            | target == node_value = True
            | otherwise = searchBFS target $ as ++ [l, r]
