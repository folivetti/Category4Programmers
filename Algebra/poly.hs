{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.List

type Algebra f a = f a -> a
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

data RingF a = RZero
             | ROne
             | RAdd a a 
             | RMul a a
             | RNeg a
             deriving (Functor, Show)
             
evalPoly :: Algebra RingF [Integer]
evalPoly RZero      = repeat 0
evalPoly ROne       = repeat 1
evalPoly (RAdd m n) = zipWith (+) m n
evalPoly (RMul m n) = zipWith (*) m n
evalPoly (RNeg n)   = fmap negate n

evalPolyFix = cata evalPoly

evalMtx :: Algebra RingF [[Integer]]
evalMtx RZero = [[0,0],[0,0]]
evalMtx ROne = [[1,1],[1,1]]
evalMtx (RAdd m n) = zipWith (zipWith (+)) m n
evalMtx (RMul m n) = zipWith (zipWith (*)) m n
evalMtx (RNeg m)  = fmap (fmap negate) m 

data StreamF e a = StreamF e a
  deriving Functor

era :: [Int] -> StreamF Int [Int]
era (p : ns) = StreamF p (filter (notdiv p) ns)
    where notdiv p n = n `mod` p /= 0

quad :: [Int] -> StreamF Int [Int]
quad (x:xs) = StreamF  (x^2) xs

quads = ana quad [0..]

toListC :: Fix (StreamF e) -> [e]
toListC = cata al
   where al :: StreamF e [e] -> [e]
         al (StreamF e a) = e : a

primes = unfoldr (\(p:ns) -> Just (p, (filter (notdiv p) ns)) ) [2..]
  where notdiv p n = n `mod` p /= 0

main = do
  print $ take 10 $ toListC $ quads
  print $ take 10 $ primes
