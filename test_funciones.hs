-- "C:/Users/David/Desktop/Haskell/practica_david/test_funciones.hs"

modificarLDL :: [[Int]] -> [[Int]]
modificarLDL [[]] = [[]]
modificarLDL xs = map modificarL xs

modificarL :: [Int] -> [Int]
modificarL [] = []
modificarL xss = map (+1) xss

data Casilla = NoMina [Int] | Mina  | Borde
   deriving (Eq,Show,Read)

type TableroBack  = [[Casilla]]
type TableroFront = [[Estado]]

data Estado = Flag  | Desc Casilla | NoDesc Casilla | Aux
   deriving (Eq,Show,Read)



modTablero :: [[Int]] -> TableroBack
modTablero xs = map modFila xs

modFila :: [Int] -> [Casilla]
modFila [] = []
modFila (x:xs)
  |x == 11 = Borde : modFila xs
  |x == 12 = Mina  : modFila xs
  |otherwise = NoMina [x] : modFila xs

modTableroFront :: [[Int]] -> TableroFront
modTableroFront xs = map modFilaFront xs

modFilaFront :: [Int] -> [Estado]
modFilaFront [] = []
modFilaFront (x:xs)
  |x == 11 = Aux : modFilaFront xs
  |x == 12 = (NoDesc Mina)  : modFilaFront xs
  |otherwise = (NoDesc (NoMina [x])) : modFilaFront xs