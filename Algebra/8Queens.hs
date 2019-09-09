{-# LANGUAGE DeriveFunctor #-}

module Main where

import System.Environment
import qualified Data.Set as S
import Algebra
import Rose

type Choices = S.Set Int

type Solution = [Int]

-- | Building the Tree
genBranch :: Coalgebra TreeF (Int, Choices)
genBranch (n, sx) = let seeds = [(m, S.delete m sx) | m <- S.toList sx]
                    in  NodeF n seeds

tree = ana genBranch

-- | converting a tree to a list of solutions
getSols :: Algebra TreeF [Solution]
getSols (NodeF n []) = [[n]]
getSols (NodeF n ts) = concat [fmap (n:) bs | bs <- ts]

removeRoot :: [Solution] -> [Solution]
removeRoot = fmap tail

unfoldTree :: Fix TreeF -> [Solution]
unfoldTree = removeRoot . cata getSols

genAllSolutions :: Int -> [Solution]
genAllSolutions n = (removeRoot . hylo getSols genBranch) (0, S.fromList [1..n])

-- | BFS
data ListF e a = EmptyF | ConsF e a
  deriving Functor

coalgBFS :: Coalgebra (ListF [Solution]) ([Solution], [Solution])
coalgBFS (xs, [[]]) = EmptyF
coalgBFS (xs, seed) = ConsF xs' (xs', seed)
  where isFeasible      = filter feasible
        cartesian ys zs = [y <> z | y <- ys, z <- zs]
        xs'             = isFeasible (cartesian xs seed)

toList EmptyF        = []
toList (ConsF xss a) = xss ++ a

nqueensBFS :: Int -> [Int]
nqueensBFS n = let seed      = fmap (:[]) [1..n]
                   sols      = hylo toList coalgBFS (seed, seed)
                   isGoal xs = length xs == n
                   xs        = (filter isGoal) sols
               in  head xs

-- |  N-Queen
-- | Check for infeasible solutions
infeasible :: Solution -> Bool
infeasible []     = False
infeasible (r:rs) = r `elem` rs || attack r rs || infeasible rs

-- | Check if a row is attacking another row of a solution
attack :: Int -> Solution -> Bool
attack r rs = r `elem` upperDiag || r `elem` lowerDiag
  where
    diag op   = zipWith op rs [1..]
    upperDiag = diag (-)
    lowerDiag = diag (+)

feasible = not . infeasible

nqueen :: Int -> Solution
nqueen = head . (filter feasible) . genAllSolutions

main = do
  [sn] <- getArgs
  
  let myTree = tree (0, S.fromList [1..3])
      n      = read sn :: Int
      
  print "Sample Tree: "
  (putStrLn.showTree) myTree

  putStrLn "\nUnfolded Tree: "
  print (unfoldTree myTree)
  
  putStrLn ("\nSolution to N-Queen for N=" ++ show n ++ ":")
  print (nqueen n)
  
  putStrLn ("\nSolution to N-Queen (bfs) for N=" ++ show n ++ ":")
  print (nqueensBFS n)
