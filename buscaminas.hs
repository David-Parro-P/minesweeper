-- Imports needed

-- "C:/Users/David/Desktop/Haskell/practica_david/buscaminas.hs"
import System.IO
import Text.Read (readMaybe)
import Data.Maybe
import Data.Char (digitToInt)

readListOfInts :: String -> [Int]
readListOfInts = read

-- los 11 son bordes, los 12 son minas
-- dificultad facil tablero 9x9 con 10 minas

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
   


-- ** DATOS **

data Casilla = NoMina [Int] | Mina  | Borde
   deriving (Eq,Show,Read)

type TableroBack = [[Casilla]]
type TableroFront = [[Estado]]

data Estado = Flag  | Desc Casilla | NoDesc Casilla | Aux -- Conseguir sustituir aux
   deriving (Eq,Show,Read)                            -- aqui o eliminarlos de front

-- ** BACK END ** 
modTableroBack :: [[Int]] -> TableroBack
modTableroBack xs = map modFilaBack xs

modFilaBack :: [Int] -> [Casilla]
modFilaBack [] = []
modFilaBack (x:xs)
  |x == 11 = Borde : modFilaBack xs
  |x == 12 = Mina  : modFilaBack xs
  |otherwise = NoMina [x] : modFilaBack xs

-- ** FRONT END **

modTableroFront :: [[Int]] -> TableroFront
modTableroFront xs = map modFilaFront xs

modFilaFront :: [Int] -> [Estado]
modFilaFront [] = []
modFilaFront (x:xs)
  |x == 11 = Aux : modFilaFront xs
  |x == 12 = (NoDesc Mina)  : modFilaFront xs
  |otherwise = (NoDesc (NoMina [x])) : modFilaFront xs
-- Mi idea es unificar los dos porque toda la informacion de back end esta en front
-- ademas solo habria que hacer una funcion de imprimir bonito para sacar el
-- output por pantalla

{-               **TRABAJAR DESCUBRIR**
-- Anadir un si la coordenada no es correcta o len incorrecta volver a introducir
descubrir :: (Int, Int) -> Tablero -> Tablero -- Cambiar bool por un tablero 
descubrir (n,m) tablero_back
   |(tablero_back!!n)!!m ==  Mina = endGame -- Muestra tablero resuelto y mensaje
   |(tablero_back!!n)!!m == NoMina[x] -- cambia estado tablero_front

-}
endGame :: Bool -- Pendiente de escribir
endGame = False 


{- 

   ** MENUS y temas de IO **
   
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



