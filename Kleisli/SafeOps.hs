module Main where

import Control.Monad

safeRoot :: Double -> Maybe Double
safeRoot x | x < 0     = Nothing
           | otherwise = Just (sqrt x)

safeReciprocal:: Double -> Maybe Double
safeReciprocal 0 = Nothing
safeReciprocal x = Just (1.0 / x)

h = safeReciprocal >=> safeRoot

main = do
  print (safeRoot 16)
  print (safeRoot (-16))
  print (h 2)
  print (h 0)
  print (h (-1))
