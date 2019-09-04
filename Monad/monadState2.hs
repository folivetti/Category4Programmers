import Control.Monad
import Control.Monad.State  
import System.Random

randomSt :: (RandomGen g) => State g Double
randomSt = state (randomR (0.0, 1.0))
                         
change b p = if p < 0.3 then not b else b
select b p = if (p < 0.3 && b) then [b] else []

mutation :: [Bool] -> State StdGen [Bool]
-- aplica o algoritmo no restante da lista, 
-- o estado atual do gerador é passado implicitamente 
-- para a função
mutation bs = sequence [fmap (change b) randomSt | b <- bs]

selection :: [Bool] -> State StdGen [Bool]
selection bs = fmap concat $ sequence $ [fmap (select b) randomSt | b <- bs]

mutThenSel bs = mutation bs >>= selection

main = do
  g <- getStdGen
  let bs = replicate 20 True
  print (fst $ runState (mutation bs) g)
  print (fst $ runState (selection bs) g)
  print (fst $ runState (mutThenSel bs) g)
