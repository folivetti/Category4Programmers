import Control.Monad
import Control.Monad.Reader

data Config = Conf { numberOfElems :: Int
                   , filtro        :: (Int -> Bool)
                   , transform     :: (Int -> Int)
                   }
                   
recupera :: Config -> String -> [Int] -> [Int]
recupera (Conf nel f t) "filtra" xs     = filtra f t nel xs
recupera (Conf nel f t) "transforma" xs = transforma t nel xs

transforma :: (Int -> Int) -> Int -> [Int] -> [Int]
transforma t nel xs = take nel (fmap t xs)

filtra :: (Int-> Bool) -> (Int -> Int) -> Int -> [Int] -> [Int]
filtra f t nel xs = let fs = filter f xs
                        ts = fmap t fs
                    in  take nel ts

recupera' :: String -> [Int] -> Reader Config [Int]
recupera' "filtra" xs     = filtra' xs
recupera' "transforma" xs = transforma' xs

filtra' :: [Int] -> Reader Config [Int]
filtra' xs = (aplicaFiltro >=> pega >=> aplicaMap) xs

aplicaFiltro :: [Int] -> Reader Config [Int]
aplicaFiltro xs = askFor filtro >>= return . filterXS
  where filterXS = (flip filter) xs

transforma' :: [Int] -> Reader Config [Int]
transforma' xs = (aplicaMap >=> pega) xs

aplicaMap :: [Int] -> Reader Config [Int]
aplicaMap xs = askFor transform >>= return . fmapXS
  where fmapXS = (flip fmap) xs
                      
pega :: [Int] -> Reader Config [Int]
pega xs = numberOfElems `bindTo` takeXS -- askFor numberOfElems >>= return . takeXS
  where takeXS = (flip take) xs

bindTo :: (Config -> a) -> (a -> b) -> Reader Config b
--bindTo p f = askFor p >>= return . f
bindTo p f = fmap f (askFor p)

askFor param = fmap param ask

myCF = Conf 10 even (^2)

main = do
  print (recupera myCF "transforma" [1..])
  print (recupera myCF "filtra" [1..])
  print (runReader (recupera' "transforma" [1..]) myCF)
  print (runReader (recupera' "filtra" [1..]) myCF)
