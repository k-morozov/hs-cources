module Foldr where

foldr' :: (a -> b ->b) -> b -> [a] -> b
foldr' f ini [] = ini
foldr' f ini (x:xs) = f x $ foldr' f ini xs

-- concatList [[1,2],[],[3]]

concatList :: [[a]] -> [a]
concatList = foldr (++) []

-- lengthList [7,6,5]

lengthList :: [a] -> Int
lengthList = foldr (const succ) 0

-- GHCi> sumOdd [2,5,30,37]
-- 42

sumOdd :: [Integer] -> Integer
sumOdd = foldr (+) 0 . filter odd


foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' k z = go
          where
            go []     = z
            go (y:ys) = k y $ go ys