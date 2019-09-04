import Data.List

class Functor w => Comonad w where
  extract :: w a -> a
  
  duplicate :: w a -> w (w a)
  duplicate = extend id
  
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  
  (=>>) :: (w a -> b) -> w a -> w b
  (=>>) = extend
  
data Neighbors a = Neig a [a] deriving Show

instance Functor Neighbors where
  fmap f (Neig a as) = Neig (f a) (fmap f as)
  
instance Comonad Neighbors where
  extract (Neig a _) = a
  duplicate (Neig a as) = Neig (Neig a as) [Neig x xs | (x:xs) <- (tail . permutations) (a:as)]
  
cosequence :: Comonad w => [w a -> a] -> w a -> w a
cosequence []     wa = wa
cosequence (f:fs) wa = cosequence fs (extend f wa)

step :: [w a -> a] -> (w a -> w a) -> w a -> w a
step fs selection wa = selection (cosequence fs wa)

search ::  (w a -> w a) -> (w a -> Bool) -> w a -> w a
search step stop wa = if   stop wa
                      then wa
                      else search step stop (step wa)
                      
bestNeig :: Ord b => (a -> b) -> Neighbors a -> a
bestNeig fitness ns = 
  where fit = fmap fitness ns
