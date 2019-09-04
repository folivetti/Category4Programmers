{-#LANGUAGE GADTs, ExplicitForAll #-}
  
module Main where

import Data.Monoid
import System.Environment

data CY f a = forall b . CY (b -> a) (f b)

--data CY f a where
--  CY :: (b -> a) -> f b -> CY f a


instance Functor (CY f) where
  fmap f (CY b2a fb)  = CY (f . b2a) fb

toCY :: f a -> CY f a
toCY = CY id

fromCY :: Functor f => CY f a -> f a
fromCY (CY f fa) = fmap f fa

withCoyo :: Functor f => (CY f a -> CY f b) -> f a -> f b
withCoyo f = fromCY . f . toCY

data Tree a = Bin a (Tree a) (Tree a) | Nil deriving (Eq, Show)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Bin x l r) = Bin (f x) (fmap f l) (fmap f r)

instance Foldable Tree where
  foldMap _ Nil = mempty
  foldMap f (Bin x l r) = f x <> foldMap f l <> foldMap f r


sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMap Sum

t :: Tree Integer
t = go 1
  where go r = Bin r (go (2*r)) (go (2*r + 1))

takeDepth :: Int -> Tree a -> Tree a
takeDepth _ Nil = Nil
takeDepth 0 _   = Nil
takeDepth d (Bin x l r) = Bin x (takeDepth (d-1) l) (takeDepth (d-1) r)

transform :: (Functor f, Num a) =>  f a -> f a
transform = fmap (^2) . fmap (+1) . fmap (*2)

printTree k = print . sumTree . takeDepth k

main :: IO ()
main = do
  args <- getArgs
  case args of
    [k] -> printTree (read k) $ transform t
    [k, "--coyo"] -> printTree (read k) $ withCoyo transform t
    _ -> error "Wrong arguments"
