module Algebra where

-- | Recursion Scheme Algebra
type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

newtype Fix f = Fix { unfix :: (f (Fix f)) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg
