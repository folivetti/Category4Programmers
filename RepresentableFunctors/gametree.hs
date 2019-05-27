{-# LANGUAGE TypeFamilies #-}

module Main where

-- Funcoes e Definicoes do problema 8-puzzle

-- Posso mover o quadrado vazio para qualquer direçao
data Moves = LFT | RGT | UP | DOWN deriving (Show, Enum, Bounded)

-- Estado contendo a coordenada da peça vazia
-- e a matriz da permutação atual
data State = S { zeroX :: Int
               , zeroY :: Int
               , board :: [[Int]]
               } deriving Show

-- Dado um estado e uma sequência de movimentos
-- faz cada movimento e chega em um estado novo
makeMoves :: State -> [Moves] -> State 
makeMoves s [] = s 
makeMoves s (mv:mvs) = makeMoves (move mv s) mvs 

-- | Executa um movimento
move :: Moves -> State -> State 
move dir (S x y b) = (S newX newY b')
  where 
    b'  = swap val b
    val | inBound newX && inBound newY = (b !! newX) !! newY
        | otherwise                    = 0         
    (newX, newY) = addTuple (x, y) (pos dir)

-- Troca um determinado valor pelo zero e vice-versa
swap :: Int -> [[Int]] -> [[Int]]
swap val = map (map change)
  where change sij | sij == val = 0 
                   | sij == 0   = val
                   | otherwise  = sij

-- Verifica se a coordenada está dentro do quadrado
inBound :: Int -> Bool
inBound x | x >= 0 && x <= 2 = True
          | otherwise        = False

-- Função auxiliar para somar coordenadas
addTuple :: Num a => (a, a) -> (a, a) -> (a, a)
addTuple (x1, x2) (y1, y2) = (x1+y1, x2+y2)

-- Mapeia um movimento para o deslocamento de coordenadas
pos :: Moves -> (Int, Int)
pos UP   = (-1,  0)
pos DOWN = ( 1,  0)
pos LFT  = ( 0, -1)
pos RGT  = ( 0,  1)

-- Estado inicial qualquer
initBoard = [[3, 2, 5], [4, 6, 0], [7, 8, 9]]
initState = S 1 2 initBoard 


-- Gametree parametrizado pelo tipo 
-- que define os possiveis movimentos
-- e o tipo do estado
data GameTree m a = Node a [GameTree m a]


class Representable f where
   type Rep f :: *
   tabulate :: (Rep f -> x) -> f x
   index    :: f x -> Rep f -> x

-- O tipo movimento tem que ter um limite e deve ser enumeravel
instance (Bounded m, Enum m) => Representable (GameTree m) where
    type Rep (GameTree m) = [m]

    -- :: (Rep Tree -> State) -> Tree State
    -- a lista de filhos da arvore é a aplicação cumulativa de 
    -- cada possível movimento 
    tabulate f = Node (f []) [tabulate (f . (mv:)) | mv <- [minBound .. maxBound]]

    -- :: Tree State -> Rep Tree -> State
    -- simplesmente percorre as ramificações 
    -- seguindo os movimentos executados
    index (Node x ts) []       = x
    index (Node x ts) (mv:mvs) = index (ts !! fromEnum mv) mvs

-- Podemos agora usar o tabulate para gerar qualquer árvore de jogos!
gameState :: (GameTree Moves State) -> [Moves] -> State
gameState t = index t  

main = do 
  let gameT = tabulate (makeMoves initState) :: GameTree Moves State
  print $ gameState gameT [UP, LFT, DOWN, DOWN, RGT]
