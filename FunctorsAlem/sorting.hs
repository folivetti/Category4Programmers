import Data.List

data Person = Person {name :: String, age :: Int} deriving Show
data Employee = Employee {tag :: Int, person :: Person} deriving Show

cmpAge :: Int -> Int -> Ordering
cmpAge x y | x < y  = LT
           | x > y  = GT
           | x == y = EQ
           
cmpPerson :: Person -> Person -> Ordering           
cmpPerson x y = cmpAge (age x) (age y)

cmpEmployee :: Employee -> Employee -> Ordering
cmpEmployee x y = cmpAge ((age. person) x) ((age.person) y)

cmap :: (b -> a) -> (a -> a -> c) -> (b -> b-> c)
cmap f c = \x y -> c (f x) (f y)

cmpPerson' = cmap age cmpAge
cmpEmployee' = cmap (age.person) cmpAge

p1 = Person "Olaf" 23
p2 = Person "George" 27
p3 = Person "Carl" 19

e1 = Employee 1234 p1
e2 = Employee 1235 p2
e3 = Employee 1236 p3

main = do
    print (sortBy cmpPerson [p1,p2,p3])
    print (sortBy cmpEmployee [e1,e2,e3])
    print (sortBy cmpPerson' [p1,p2,p3])
    print (sortBy cmpEmployee' [e1,e2,e3])
