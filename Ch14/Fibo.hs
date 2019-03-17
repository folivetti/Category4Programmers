{-# LANGUAGE TypeFamilies #-}

module Main where

import System.Environment

data Stream x = Cons x (Stream x)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

class Representable f where
   type Rep f :: *
   tabulate :: (Rep f -> x) -> f x
   index    :: f x -> Rep f -> x
   
instance Representable Stream where
  type Rep Stream = Integer
  tabulate f = Cons (f 0) (tabulate (f . (+1)))
  index (Cons b bs) n | n == 0    = b
                      | otherwise = index bs (n-1)
    
f :: Integer -> Integer
f 0 = 0
f 1 = 1
f n = f (n-1) + f (n-2)

t :: Stream Integer
t = tabulate f

main :: IO ()
main = do
  args <- getArgs
  case args of
    [k] -> do
              let i = read k
              print $ f i
              print $ f i
    [k, "--rep"] -> do
                      let i = read k
                      print $ index t i
                      print $ index t i  
    _ -> error "Wrong arguments"
