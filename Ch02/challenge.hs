module Main where

import System.IO.Unsafe
import System.Random

{-
 - Using ghci:
 - ghci challenge.hs
 - > :set +s
 - > fibo 30
 - > mfibo = memoize fibo
 - > mfibo 30
 - > mfibo 30
 -}

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0..] !!)

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

randomTill :: Int -> Int
randomTill n = unsafePerformIO $ randomRIO (0,n)

randomSeed seed = do
                  let g = mkStdGen seed
                  return $ random g

main = print "Begin!"

