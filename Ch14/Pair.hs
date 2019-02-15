{-# LANGUAGE TypeFamilies #-}

module Main where

class Representable f where
   type Rep f :: *
   tabulate :: (Rep f -> x) -> f x
   index    :: f x -> Rep f -> x
   
data Pair a = Pair a a

instance Representable Pair where
    type Rep Pair = Bool
    tabulate f = Pair (f True) (f False)
    index (Pair x1 x2) b = if b then x1 else x2
    
f :: Bool -> String
f True = "Ok!"
f False = "Error!"

t :: Pair String
t = tabulate f

main = do
  print (index t True)
  print (index t False)
