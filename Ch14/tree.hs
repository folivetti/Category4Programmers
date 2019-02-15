{-# LANGUAGE TypeFamilies #-}

module Main where


data Tree a = Node a [Tree a]

type Moves = (Integer, Integer)

data Tics = X | O | Nada deriving (Show, Eq)

type State = [[Tics]]

initState = [[Nada, Nada, Nada],
             [Nada, Nada, Nada],
             [Nada, Nada, Nada]]

validMoves = [(i,j) | i <- [0..2], j <- [0..2]]

mov2idx mov = fst . head $ filter (\(i, x) -> x==mov) $ zip [0..] validMoves

class Representable f where
   type Rep f :: *
   tabulate :: (Rep f -> x) -> f x
   index    :: f x -> Rep f -> x

instance Representable Tree where
    type Rep Tree = [Moves]

    -- :: (Rep Tree -> State) -> Tree State
    tabulate f = Node (f []) [tabulate (f . (mv:)) | mv <- validMoves]

    -- :: Tree State -> Rep Tree -> State
    index (Node x ts) []       = x
    index (Node x ts) (mv:mvs) = index (ts !! mov2idx mv) mvs


makeMoves :: State -> [Moves] -> State
makeMoves s [] = s
makeMoves s (mv:mvs) = makeMoves (move mv s) mvs

replace p f xs = [ if i == p then f x else x | (x, i) <- zip xs [0..] ]

move (x,y) s = replace y (replace x (const mv)) s
  where mv = nextMv s


nextMv s = if even (length nadas) then X else O
  where nadas = filter (/=Nada) $ concat s

gameStates :: [Moves] -> State
gameStates = index t 
  where t = tabulate (makeMoves initState) :: Tree State


main = print $ gameStates [(1,0), (0,0), (2,2), (1,1)]
