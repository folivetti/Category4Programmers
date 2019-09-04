import Control.Monad
import Control.Monad.State  
import System.Random

randomSt :: (RandomGen g) => State g Double
randomSt = state (randomR (0.0, 1.0))

rule :: Double -> State StdGen (Bool -> Bool)
rule p = if p < 0.3 then return not else return id

mutatePoint :: State StdGen (Bool -> Bool)
mutatePoint = randomSt >>= rule

mutation'' :: [Bool] -> State StdGen [Bool]
mutation'' bs = state (\s -> (zipWith ($) (fst $ runState fs s) bs, s))
  where fs = sequence [mutatePoint | _ <- bs]

changeAndconcat :: Bool -> [Bool] -> Double -> [Bool]
changeAndconcat b bs p = if p < 0.3
                         then  (not b : bs)
                         else  (b : bs)
                         
mutation :: [Bool] -> State StdGen [Bool]
-- se a lista estiver vazia, nada a fazer
mutation [] = return []

-- aplica o algoritmo no restante da lista, 
-- o estado atual do gerador é passado implicitamente 
-- para a função
mutation (b:bs) = (mutation >=> change) bs
  where
     change bs' = randomSt >>= toState (changeAndconcat b bs')
     toState f  = \x -> return (f x)

         
mutation' :: [Bool] -> State StdGen [Bool]
mutation' [] = return []

mutation' (b:bs) = do
    bs' <- mutation' bs
    p   <- randomSt
    if p < 0.3
    then return (not b : bs')
    else return (b : bs')

main = do
  g <- getStdGen
  print (fst $ runState (mutation (replicate 20 True)) g)
  print (fst $ runState (mutation' (replicate 20 True)) g)
  print (fst $ runState (mutation'' (replicate 20 True)) g)
