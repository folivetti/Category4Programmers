class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap g (Store f s) = Store (g . f) s

instance Comonad (Store s) where
  extract (Store f s)   = f s
  duplicate (Store f s) = Store (Store f) s

instance (Num s, Enum s, Show a) => Show (Store s a) where
  show (Store f s) = unwords $ fmap show $ [f (s+i) | i <- [-10 .. 10]]

rule :: Int -> Store Int Int -> Int
rule x (Store f s) = (succDiv2 !! bit) `rem` 2
  where bit      = (f (s+1)) + 2*(f s) + 4*(f (s-1))
        succDiv2 = iterate (`div` 2) x

f0 :: Int -> Int
f0 0 = 1
f0 _ = 0

fs :: Store Int Int
fs = Store f0 0

wolfram :: (Store Int Int -> Int) -> Store Int Int -> [Store Int Int]
wolfram rl = iterate (extend rl)


main = do
  let wolf30 = wolfram (rule 30) fs
  putStr (unlines $ fmap show $ take 5 wolf30)
