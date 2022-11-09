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

comprobarAccion :: [Char] -> Bool
comprobarAccion letras
   |letras == "flag" = True
   |otherwise = False

-- Jugar bucle
nameReturn :: IO String
nameReturn = do putStr "What is your first name? "
                first <- getLine
                putStr "And your last name? "
                last <- getLine
                let full = first ++ " " ++ last
                return full
greetAndSeeYou :: IO ()
greetAndSeeYou = do name <- nameReturn
                    putStrLn ("See you, " ++ name ++ "!")

main :: IO ()
main = do
-- lectura y tratamiento
   let ruta = "C:/Users/David/Desktop/Haskell/practica_david/tests.txt"
   contenido <- readFile ruta
   let tablero = map (map (read::String->Int)) (map words (lines contenido))
   let tableroFrontEnd = modTableroFront tablero
-- asignaciones
   accion <- bucleAccion
   n      <- bucleCoordenada
   m      <- bucleCoordenadaDos
   putStrLn "Hola!"

{- 
   Corregir: deja meter un string de un caracter como coordenada
   No puede meter dos ints como coordenada tipo 10 
   me gustaria otro salto de linea por claridad
-}


bucleAccion :: IO String
bucleAccion = do
   putStrLn "¿Que accion quieres hacer? (bandera/b) o (descubrir/d)"
   accion <- getLine
   if accion == "bandera" || accion == "b" || accion == "descubrir" || accion == "d"
   then return accion
   else putStrLn "No es una accion correcta" >> bucleAccion

bucleCoordenada :: IO String
bucleCoordenada = do
   putStrLn "Elige una primera coordenada (numero natural)"
   primera <- getLine
   if length primera == 1
   then return primera
   else putStrLn "No es una coordenada correcta" >> bucleCoordenada

bucleCoordenadaDos :: IO String
bucleCoordenadaDos = do
   putStrLn "Elige una segunda coordenada (numero natural)"
   segunda <- getLine
   if length segunda == 1
   then return segunda
   else putStrLn "No es una coordenada correcta" >> bucleCoordenadaDos


{-
    ** DESCUBRIR **
-}

