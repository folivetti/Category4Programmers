class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  (=>>) :: (w a -> b) -> w a -> w b
  (=>>) = extend
  
data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons a as) = Cons (f a) (fmap f as)
  
instance Comonad Stream where
  extract (Cons a _) = a
  duplicate (Cons a as) = Cons (Cons a as) (duplicate as)
  
sumN :: Num a => Int -> Stream a -> a
sumN n (Cons a as) | n <= 0    = 0
                   | otherwise = a + sumN (n-1) as
                   
avgN :: Fractional a => Int -> Stream a -> a
avgN n as = (sumN n as) / (fromIntegral n)

movAvg :: Fractional a => Int -> Stream a -> Stream a
movAvg n as = (avgN n) =>> as

takeN :: Int -> Stream a -> [a]
takeN n (Cons a as) | n == 0    = []
                    | otherwise = a : (takeN (n-1) as)

next :: Double -> Stream Double
next x = Cons (x + 1.0) (next (x+1.0))

stream :: Stream Double
stream = Cons 1.0 (next 1.0)
