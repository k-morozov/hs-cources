module Monade where

data Log a = Log [String] a
	deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg v = Log [msg] (f v)


execLoggers v f g =  Log (xs ++ xss)  v'' where
                     Log xs v' = f v
                     Log xss v'' = g v'
