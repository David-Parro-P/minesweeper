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

-- Print testers

{- Colores 
   1: Azul
   2: Verde
   3: Rojo
   4: Morado
   5: magenta brillante
   6: Amarillo
   7: Cyan
   8: Gris
   9: Negro
-}

-- Usa mapM_ para printear [[Char]] con putStrLn


-- meter en la main para que no tengas que poner el IO  
--imprimirTablero = TableroFront -> IO ()
--imprimirTablero tablero = mapM_ putStrLn (dibujarTablero tablero)

dibujarTablero :: TableroFront -> [[Char]]
dibujarTablero board = map dibujarLinea board


dibujarLinea :: [Estado] -> [Char]
dibujarLinea [] = ""
dibujarLinea (x:xs)
   |x == Aux = dibujarLinea xs
   |x == Flag = "\ESC[0m!" ++ dibujarLinea xs
   |x == NoDesc(Mina) = "\ESC[0m*" ++ dibujarLinea xs
   |elem x listNoDesc = "\ESC[0m*" ++ dibujarLinea xs
   |otherwise = sacarNumDesc x ++ dibujarLinea xs
  where listNoDesc = [NoDesc(NoMina[i]) | i <- [0..9]]

sacarNumDesc :: Estado -> [Char]
sacarNumDesc (Desc (x)) = dibujarCasilla x


dibujarCasilla :: Casilla -> [Char]
dibujarCasilla Borde = []
dibujarCasilla Mina = "\ESC[0mx"
dibujarCasilla (NoMina[x]) = colores!!x ++ show x
   where colores = ["\ESC[37m", "\ESC[34m", "\ESC[92m", "\ESC[91m", "\ESC[35m", "\ESC[95m", "\ESC[33m", "\ESC[96m", "\ESC[90m", "\ESC[37m"]
