import Control.Monad
import Control.Monad.Reader

data Config = Conf { alg :: String
                   , thr :: Double
                   , it  :: Int
                   }

go :: Int -> [Double] -> [Double]
go _ []     = []
go 0 xs     = xs
go i (x:xs) = (i' * x) : go (i-1) xs
  where i' = fromIntegral i

filterLess thr xs = filter (<thr) xs

f2 :: Config -> [Double] -> [Double]
f2 cfg xs = f3 cfg $ filterLess (thr cfg) xs

f3 :: Config -> [Double] -> [Double]
f3 cfg xs = go (it cfg) xs

algorithm :: Config -> [Double] -> [Double]
algorithm cfg xs | alg cfg == "f2" = f2 cfg xs
                 | otherwise       = f3 cfg xs

type ConfReader = Reader Config [Double]

f3' :: [Double] -> Reader Config [Double]
f3' xs = do
  t <- fmap it ask
  return (go t xs)

f2' :: [Double] -> Reader Config [Double]
f2' xs = do
  t  <- fmap thr ask
  f3' (filterLess t xs)

algorithm' :: [Double] -> Reader Config [Double]
algorithm' xs = do
  alg <- fmap alg ask
  if alg == "f2" then f2' xs else f3' xs

f3'' :: [Double] -> Reader Config [Double]
f3'' xs = fmap it ask >>= runGo
  where runGo t = return (go t xs)

f2'' :: [Double] -> Reader Config [Double]
f2'' xs = fmap thr ask >>= gof3
  where gof3 t = f3'' (filterLess t xs)


algorithm'' :: [Double] -> Reader Config [Double]
algorithm'' xs = fmap alg ask >>= choice
  where choice "f2" = f2'' xs
        choice _    = f3'' xs
--  where choice a | a == "f2" = f2'' xs
--                 | otherwise = f3'' xs

main = do
  let xs = [1.0, 2.0 .. 10.0]
      c = Conf "f2" 2.5 5
  print (algorithm c xs)
  print (runReader (algorithm' xs) c)
  print (runReader (algorithm'' xs) c)

