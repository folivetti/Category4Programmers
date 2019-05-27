module Main where

import Control.Monad.Cont

soma :: Int -> Int -> (Cont r Int)
soma x y = return (x+y)

dobra :: Int -> (Cont r Int)
dobra x = return (2*x)

fib :: Int -> (Cont r Int)
fib 0 = return 0
fib 1 = return 1
fib n = do x <- fib (n-1)
           y <- fib (n-2)
           return (x+y)
           
main = do
  runCont (soma 2 3 >>= dobra) print
  runCont (fib 6) print
