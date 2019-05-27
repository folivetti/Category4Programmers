module Main where

import Control.Monad.Cont
import System.Environment

sat n = runCont $ sequence $ replicate n $ cont $ \k -> k $ k True

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> print $ sat (read n :: Int) (\bs -> and bs)
    _ -> error "Usage ./sat n"
