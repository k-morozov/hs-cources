{-# LANGUAGE InstanceSigs #-}
module ClassType where

data Tree a = Leaf 
            | Node (Tree a) a (Tree a) 
--   deriving Show

instance Eq a => Eq (Tree a) where
    (==) :: Eq a => Tree a -> Tree a -> Bool
    Leaf == Leaf = True
    (Node a b c) == (Node x y z)
        | b /= y = False
        | otherwise = smartCmp [a] [x] && smartCmp [c] [z]
        where
            smartCmp :: Eq a => [Tree a] -> [Tree a] -> Bool
            smartCmp [] [] = True
            smartCmp (Leaf:as) (Leaf:bs) = smartCmp as bs
            smartCmp ((Node a b c):as) ((Node x y z):bs)
                | b /= y = False
                | otherwise = smartCmp (as ++ [a, x]) (bs ++ [c, z])
            smartCmp _ _ = False
    _ == _ = False

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f tree = case tree of
        Leaf -> Leaf
        (Node l v r) -> Node (fmap f l) (f v) (fmap f r)

instance Show a => Show (Tree a) where
--   showsPrec :: Show a => Int -> Tree a -> ShowS
  showsPrec n tree = case tree of
    Leaf -> \x -> x ++ "{}"
    (Node l v r) -> \x -> x ++ "<" ++ (showsPrec n l "") ++ (showsPrec n v) (showsPrec n r "") ++  ">"



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
        searchBFS target (a:as) = case a of
            Leaf -> searchBFS target as
            (Node l node_value r) -> (target == node_value) || searchBFS target (as ++ [l, r])

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D a1 a2 a3) = Point3D (f a1) (f a2) (f a3) 


data Tree' a = Leaf' (Maybe a) | Branch (Tree' a) (Maybe a) (Tree' a) 
    deriving Show

instance Functor Tree' where
    fmap f (Leaf' a) = case a of
        Nothing -> Leaf' Nothing
        (Just value) -> Leaf' $ Just $ f value

    fmap f (Branch lhs v rhs) = Branch (f <$> lhs) (helper f v) (f <$> rhs) 
        where 
            helper f v = case v of
                Nothing ->  Nothing
                (Just value) -> Just $ f value
