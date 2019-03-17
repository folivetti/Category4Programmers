import Control.Monad
import Control.Monad.State  
import System.Random

randomSt :: (RandomGen g) => State g Double
randomSt = state (randomR (0.0, 1.0))

mutation :: [Bool] -> State StdGen [Bool]
mutation [] = return []

mutation (b:bs) = (mutation >=> choice) bs
  where
     choice bs' = randomSt >>= concat bs'
     concat bs' p = if p < 0.3
                    then return (not b : bs')
                    else return (b : bs')
         
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
