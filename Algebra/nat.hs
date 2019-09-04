module Main where

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

fat :: NatF (Integer, Integer) -> (Integer, Integer)
fat ZeroF = (1, 1)
fat (SuccF (m, n)) = (m+1, m*n)

catafib :: Fix NatF -> (Integer, Integer)
catafib = cata fib

catafat :: Fix NatF -> (Integer, Integer)
catafat = cata fat

main = do
  print $ catafib (n2nat 10)
  print $ catafat (n2nat 5)
