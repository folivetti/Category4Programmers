{-# LANGUAGE DeriveFunctor #-}

module Main where

type Algebra f a = f a -> a
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

data RingF a = RZero
             | ROne
             | RAdd a a 
             | RMul a a
             | RNeg a
             deriving (Functor, Show)
             
evalZ :: Algebra RingF Integer
evalZ RZero      = 0
evalZ ROne       = 1
evalZ (RAdd m n) = m + n
evalZ (RMul m n) = m * n
evalZ (RNeg n)   = -n

evalFix :: Fix RingF -> Integer
evalFix (Fix RZero) = 0
evalFix (Fix ROne)  = 1
evalFix (Fix (RAdd e1 e2)) = evalFix e1 + evalFix e2
evalFix (Fix (RMul e1 e2)) = evalFix e1 * evalFix e2
evalFix (Fix (RNeg e)) = -(evalFix e)

evalFix2 = cata evalZ

expr :: Fix RingF
expr = Fix (RAdd (Fix (RMul (Fix ROne) (Fix ROne))) (Fix (RMul (Fix RZero) (Fix RZero))))

main = print $ evalFix2 expr
