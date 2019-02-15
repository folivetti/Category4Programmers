module Main where

f :: Int -> (Int, Int)
f x = (x, x)

g :: (Int, Int) -> String
g (x,y) = show x ++ " - " ++ show y

h = g . f

main = print (h 3)
