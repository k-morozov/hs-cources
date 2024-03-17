module DataType
    ( 
        someFunc
        , cmp
        , LogLevel(..)
    ) where

import Data.Function

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
        