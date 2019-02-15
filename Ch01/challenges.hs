module Main where

myId :: a -> a
myId x = x

myCompose :: (b -> c) -> (a -> b) -> (a -> c)
myCompose g f = \x -> g (f x)

times2 :: Int -> Int
times2 x = 2*x

main = do
  let range = [0..100]
      comp1 = map (myCompose myId times2) range
      comp2 = map (myCompose times2 myId) range
      justf = map times2 range
  print $ comp1 == justf
  print $ comp2 == justf 
