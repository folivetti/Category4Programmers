{-# LANGUAGE DeriveFunctor #-}

module Rose where

import Algebra

-- | Search Tree Structure
data Rose = NodeR Int [Rose]
  deriving Show

data TreeF a = NodeF Int [a]
  deriving (Functor, Show)

type Tree = Fix TreeF

-- | From drawtree in Data.Tree 
showTree = unlines . drawTree

drawTree (Fix (NodeF x ts)) = show x : drawSubTrees ts
  where drawSubTrees [] = []
        drawSubTrees [t] = "|" : shift "`- " "   " (drawTree t)
        drawSubTrees (t:ts) = "|" : shift "+- " "|  " (drawTree t) ++ drawSubTrees ts
        shift first other = zipWith (++) (first : repeat other)
