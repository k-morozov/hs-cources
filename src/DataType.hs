module DataType
    ( 
        someFunc
        , cmp
        , LogLevel(..)
    ) where

import Data.Function
import Data.Char(isDigit)


someFunc :: Int -> Int
someFunc x = x

-- lecture 1 part 1

-- Bool - конструктор типа или имя типа, величина внутри типа
-- True - конструктор данных
-- data Bool = True | False

data B = T | F
    deriving (Show,Eq,Enum)

not' :: B -> B
not' T = F
not' F = T

-- Тип данных Color определен следующим образом
data Color = Red | Green | Blue
-- Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.
-- GHCi> show Red
-- "Red"

instance Show Color where
    show x = case x of
        Red -> "Red"
        Green -> "Green"
        Blue -> "Blue"


-- Тип LogLevel описывает различные уровни логирования.

data LogLevel = Error | Warning | Info
 

-- Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.
-- GHCi> cmp Error Warning
-- GT
-- GHCi> cmp Info Warning
-- LT
-- GHCi> cmp Warning Warning
-- EQ

instance Enum LogLevel where
    fromEnum Error = 2
    fromEnum Warning = 1
    fromEnum Info = 0

    toEnum 2 = Error
    toEnum 1 = Warning
    toEnum 0 = Info
    toEnum _ = error "no enum"

-- instance Eq LogLevel where
--     l == r = fromEnum l == fromEnum r


-- instance Ord LogLevel where
--     l <= r = fromEnum l <= fromEnum r

cmp :: LogLevel -> LogLevel -> Ordering
cmp = on compare fromEnum
        

data Point = Pt Double Double
-- data Point = Point Double Double

origin :: Point
origin = Pt 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Pt x y) = sqrt (x ^ 2 + y ^ 2)

data Roots = Roots Double Double | None
    deriving Show

roots :: Double -> Double -> Double -> Roots
roots a b c 
    | discr >=0 = Roots x1 x2
    | otherwise = None
    where
        discr = b ^ 2 - 4 * a * c
        x1 = helper d
        x2 = helper (-d)
        helper x = (-b + x) / (2 * a)
        d = sqrt discr


-- Определим тип фигур Shape:

data Shape = Circle Double | Rectangle Double Double
-- У него два конструктора: Circle r — окружность радиуса r, и Rectangle a b — прямоугольник с размерами сторон a и b. 
-- Реализуйте функцию area, возвращающую площадь фигуры. Константа pi уже определена в стандартной библиотеке.

area :: Shape -> Double
area shape = case shape of
    (Circle r) -> pi * r ^ 2
    (Rectangle a b) -> a * b

data Result' = Fail' Int | Success' 
data Result = Fail | Success

instance Show Result' where
    show (Fail' n) = "Fail: " ++ show n
    show _ = "Success"

-- doSomeWork' :: SomeData -> Result'
-- doSomeWork' x = case doSomeWork x of
--                     (Success, _) -> Success'
--                     (_, n) -> Fail' n

(***) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(***) f g ~(a, b) = (f a, g b)
-- const 4 *** const 2 $ undefined

data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving Show
-- firstName p
-- p & firstName

-- f $ g $ h $ x = f (g(h(x)))
-- x & h & g & f

updateAge :: Int -> Person -> Person
updateAge newAge person = person { age = newAge}


updateLastName :: Person -> Person -> Person
updateLastName (Person {lastName = ln}) person2 = person2 { lastName = ln}
-- updateLastName (Person _ ln1 _) p2 = p2 {lastName = ln1}

isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False


abbrFirstName :: Person -> Person
abbrFirstName p@(Person{firstName = name})
    | length name < 2 = p
    | otherwise = p{firstName = take 1 name ++ "."}


roots' :: Double -> Double -> Double -> Either error Roots
roots' a b c 
    | discr >=0 = Right $ Roots x1 x2
    | otherwise = Left $ error "no roots. descr less 0"
    where
        discr = b ^ 2 - 4 * a * c
        x1 = helper d
        x2 = helper (-d)
        helper x = (-b + x) / (2 * a)
        d = sqrt discr


findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs)
    | isDigit x = Just x
    | otherwise = findDigit xs

findDigitOrX :: [Char] -> Char
findDigitOrX as = case findDigit as of
    (Just a) -> a
    Nothing -> 'X'

-- findDigitOrX = fromMaybe 'X' . findDigit

maybeToList :: Maybe a -> [a]
maybeToList value = case value of
    (Just a) -> [a]
    Nothing -> []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

data Coord a = Coord a !a
    deriving Show

getX :: Coord a -> a
getX (Coord x _) = x

getY :: Coord a -> a
getY (Coord _ y) = y

data List a = Nil | Cons a (List a)
    deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a b) = a:fromList b

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x $ toList xs


-- GHCi> let tree = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)
-- GHCi> (treeSum tree, treeHeight tree)
-- (10,3)

data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a) 
  deriving Show

treeSum :: Tree' Integer -> Integer
treeSum Leaf' = 0
treeSum (Node' lhs value rhs) = value + on (+) treeSum lhs rhs

treeHeight :: Tree' a -> Int
treeHeight Leaf' = 0
treeHeight (Node' lhs value rhs) = 1 + on max treeHeight lhs rhs


-- another task

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node lhs rhs) = 1 +  on max height lhs rhs

size :: Tree a -> Int
size (Leaf _) = 1
size (Node lhs rhs) = 1 + on (+) size lhs rhs