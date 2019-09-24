{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.List
import Control.Monad
import Control.Monad.State
import System.Random

randomSt :: (RandomGen g) => State g Double
randomSt = state (randomR (0.0, 1.0))

type Algebra f a = f a -> a
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

data StreamF e a = StreamF e a
  deriving Functor
  
mut :: [Bool] -> StreamF (State StdGen Bool) [Bool]
mut (b:bs) = let change bi p = if p < 0.3 then not bi else bi
                 b'          = fmap (change b) randomSt 
             in  StreamF b' bs

toListC :: Fix (StreamF e) -> [e]
toListC = cata al
   where al :: StreamF e [e] -> [e]
         al (StreamF e a) = e : a
        
        
-- Trees
data TreeF e a = NodeF (Expr e) a a | LeafF 
               deriving Functor

data Tree e = Node (Expr e) (Tree e) (Tree e) | Leaf 
       deriving (Functor, Show)

data Expr e = Fun String | Op String | Const e | Var Int
       deriving (Functor, Show)
       
choose :: Double -> Expr Double
choose p | p < 0.2   = Op "+"
         | p < 0.4   = Fun "sin"
         | p < 0.6   = Var 1
         | otherwise = Const 0.3
    
rndExpr :: StdGen -> (Expr Double, StdGen, StdGen)
rndExpr s = let (p, s')  = randomR (0.0, 1.0) s :: (Double, StdGen)
                (s1, s2) = split s'
            in  (choose p, s1, s2)

mutTree :: (Bool, StdGen) -> TreeF Double (Bool, StdGen)
mutTree (False, s) = LeafF 
mutTree (True, s) = let (e, s1, s2)  = rndExpr s
                    in  case e of
                          Op  name -> NodeF e (True, s1) (True, s2)
                          Fun name -> NodeF e (True, s1) (False, s2)
                          Var x    -> NodeF e (False, s2) (False, s2)
                          Const x  -> NodeF e (False, s2) (False, s2)
toTreeC = cata al
   where al :: TreeF e (Tree e) -> Tree e
         al (NodeF e l r) = Node e l r
         al LeafF         = Leaf

main = do  
  g <- getStdGen
  let mutated = ana mut (repeat True) --((repeat True), g)
      myBool  = sequence $ toListC $ mutated
      myTree  = ana mutTree (True,  g)
  print $ take 10 $ fst $ runState myBool g
  print $ toTreeC $ myTree-- ana mutTree (Op "-",g)
  --print $ fst $ runState myTree g
