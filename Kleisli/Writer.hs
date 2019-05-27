type Writer a = (a, String)

return :: a -> Writer a
return x = (x, "")

(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
m1 >=> m2 = \a ->
    let (b, s1) = m1 a
        (c, s2) = m2 b
    in (c, s1 ++ s2)
    
notW :: Bool -> Writer Bool
notW b = (b, "not")

is_even :: Int -> Writer Bool
is_even x = (x `mod` 2 == 0, "even")

is_odd :: Int -> Writer Bool
is_odd = is_even >=> notW

main = print (is_odd 7)
