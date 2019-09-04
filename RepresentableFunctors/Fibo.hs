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

{- F-Algebra -}
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

data NatF a = ZeroF | SuccF a

instance Functor NatF where
  fmap f ZeroF = ZeroF
  fmap f (SuccF x) = SuccF (f x)

n2nat :: Integer -> Fix NatF
n2nat 0 = Fix ZeroF
n2nat x = Fix (SuccF (n2nat (x-1)))

fib :: NatF (Integer, Integer) -> (Integer, Integer)
fib ZeroF = (0, 1)
fib (SuccF (m, n)) = (n, m + n)

catafib :: Fix NatF -> (Integer, Integer)
catafib = cata fib

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
    [k, "--alg"] -> do
                      let i = read k
                      print $ catafib (n2nat i)
                      print $ catafib (n2nat i) 
    _ -> error "Wrong arguments"
