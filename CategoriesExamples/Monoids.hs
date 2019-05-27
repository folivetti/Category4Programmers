class Monoid' m where
    mempty'  :: m
    mappend' :: m -> m -> m
    
instance Monoid' Int where
    mempty' = 1
    mappend' = (*)
    
main = do
  let x = 2 :: Int
      y = 3 :: Int
  print (mappend' x y)
