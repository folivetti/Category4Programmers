data Shape = Circle Float
           | Rect Float Float
           | Square Float
           deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square s) = s * s

circ :: Shape -> Float
circ (Circle r) = 2.0 * pi * r
circ (Rect d h) = 2.0 * (d + h)
--circ (Square s) = 4.0 * s

main = do
    print (area (Circle 2))
