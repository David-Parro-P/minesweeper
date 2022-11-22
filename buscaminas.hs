-- Imports needed

-- "C:/Users/David/Desktop/progDeclarativa/Haskell/minesweeper-main/buscaminas.hs"
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
   let ruta = "tests.txt"
   contenido <- readFile ruta
   let tablero = map (map (read::String->Int)) (map words (lines contenido))
   let tableroFrontEnd = modTableroFront tablero
   bucleJuego tableroFrontEnd


-- ** DATOS **

data Casilla = NoMina [Int] | Mina  | Borde
   deriving (Eq,Show,Read)

type TableroBack = [[Casilla]]
type TableroFront = [[Estado]]

data Estado = Flag Casilla | Desc Casilla | NoDesc Casilla | Aux -- Conseguir sustituir aux
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






-- hay que hacer la accion en la priemra llamada con un int = 0
-- los dos primeros ints son unos contadores de posicion
-- la segunda es el resultado de la funcion buscar ceros que devuelve la posicion

buscarCerosTablero :: (Int,Int) -> TableroFront -> [(Int,Int)]
buscarCerosTablero _ [[]] = []
buscarCerosTablero (n,m) tablero
   |tablero!!0 == [] = []
   |a == length(tablero)                 = []
   |b >= length(tablero!!0)              = (buscarCerosTablero (a+1,0) tablero)

   |(tablero!!a)!!b == (Desc(NoMina[0])) = (a,b) : (buscarCerosTablero (a,b+1) tablero)
   |otherwise = buscarCerosTablero (a,b+1) tablero
   where a = fst (n,m)
         b = snd (n,m)

flipTresMedioDcha :: (a -> b -> c -> d) -> a -> c -> b -> d
flipTresMedioDcha f x z y = f x y z

-- la funcion que busca ceros despues de cada loop es pasar 
-- map (flipTresMedioDcha descubrirCeros (0,0) tablero2) (buscarCerosTablero (0,0) tablero2)

limpiarTableroFeliz :: TableroFront -> TableroFront
limpiarTableroFeliz  tablero = repetirN limpiarCerosFront tablero (length tablero)

limpiarCerosFront :: TableroFront -> TableroFront
limpiarCerosFront tablero   =  f listaDeIncidencias
   where listaDeIncidencias = map listIncidencia (buscarCerosTablero (0,0) tablero)
         f                  = limpiarCerosTablero tablero

repetirN :: (a -> a) -> a -> Int -> a
repetirN f x n  = iterate f x !! n

-- Reescribir como FOLDR !!!!!

limpiarCerosTablero :: TableroFront -> [[(Int,Int)]] -> TableroFront
limpiarCerosTablero tablero []   = tablero
limpiarCerosTablero tablero (x:xs) = limpiarCerosTablero (limpiarCeros tablero x) xs 


limpiarCeros :: TableroFront -> [(Int,Int)] -> TableroFront
limpiarCeros tablero []     = tablero
limpiarCeros tablero (x:xs) = limpiarCeros (descubrir 0 x tablero) xs

listIncidencia :: (Int,Int) -> [(Int,Int)]
listIncidencia (n,m) = [(n+x,m+y) | x <- [-1,0,1], y <- [-1,0,1]]  



descubrir :: Int -> (Int,Int) -> TableroFront -> TableroFront
descubrir contador (n,m) [] = []
descubrir contador (n,m) (x:xs)
   |contador == n = (descubrirFila (n,m) x) : xs 
   |otherwise = x : (descubrir (contador+1) (n,m) xs)

descubrirFila :: (Int,Int) -> [Estado] -> [Estado] 
descubrirFila (n,m) fila = primera ++ [descubrirCasilla posicion] ++ tail(segunda)
   where primera = fst(splitAt m (fila))
         segunda = snd(splitAt m (fila))
         posicion = (fila)!!m

descubrirCasilla :: Estado -> Estado
descubrirCasilla (Desc(x)) = Desc(x)
descubrirCasilla (Flag(x)) = Flag(x)
descubrirCasilla Aux = Aux
descubrirCasilla (NoDesc(Mina)) = (Desc(Mina))
descubrirCasilla (NoDesc(NoMina[x]))
   |x == 0 = Desc(NoMina[x])
   |otherwise = (Desc(NoMina[x]))

-- Para ensenar todo endGame


descubrirTodoTest :: TableroFront -> [(Int,Int)] -> TableroFront
descubrirTodoTest tablero [] = tablero
descubrirTodoTest tablero (x:xs) = descubrirTodoTest (descubrir 0 x tablero) xs

descubrirTodo :: TableroFront -> TableroFront
descubrirTodo tablero = descubrirTodoTest tablero casillas
   where casillas = [(x,y) | x <- [1..(length tablero - 2)] , y <- [1..(length (tablero!!0) - 2)]]
{- ** FLAG ** -}


bandera :: Int -> (Int,Int) -> TableroFront -> TableroFront
bandera contador (n,m) [] = []
bandera contador (n,m) (x:xs)
   |contador == n = (banderaFila (n,m) x) : xs 
   |otherwise = x : (bandera (contador+1) (n,m) xs)

banderaFila :: (Int,Int) -> [Estado] -> [Estado] 
banderaFila (n,m) fila = primera ++ [banderaCasilla posicion] ++ tail(segunda)
   where primera = fst(splitAt m (fila))
         segunda = snd(splitAt m (fila))
         posicion = (fila)!!m

banderaCasilla :: Estado -> Estado
banderaCasilla (Desc(x)) = Desc(x)
banderaCasilla (Flag(x)) = NoDesc(x)
banderaCasilla Aux = Aux
banderaCasilla (NoDesc(x)) = (Flag(x))

{- ** Pretty Prints **
-}

dibujarTablero :: TableroFront -> [[Char]]
dibujarTablero board = map dibujarLinea board


dibujarLinea :: [Estado] -> [Char]
dibujarLinea [] = ""
dibujarLinea (x:xs)
   |x == Aux = dibujarLinea xs
   |x == (Flag(Mina))      = "\ESC[31m!" ++ dibujarLinea xs
   |elem x listNoDescFlag  = "\ESC[31m!" ++ dibujarLinea xs
   |x == NoDesc(Mina)      = "\ESC[0m*" ++ dibujarLinea xs
   |elem x listNoDesc      = "\ESC[0m*" ++ dibujarLinea xs
   |otherwise = sacarNumDesc x ++ dibujarLinea xs
  where listNoDesc     = [NoDesc(NoMina[i]) | i <- [0..9]]
        listNoDescFlag = [Flag(NoMina[i]) | i <- [0..9]]

sacarNumDesc :: Estado -> [Char]
sacarNumDesc (Desc (x)) = dibujarCasilla x


dibujarCasilla :: Casilla -> [Char]
dibujarCasilla Borde = []
dibujarCasilla Mina = "\ESC[31mx\ESC[0m"
dibujarCasilla (NoMina[x]) = colores!!x ++ show x
   where colores = ["\ESC[37m", "\ESC[34m", "\ESC[92m", "\ESC[91m", "\ESC[35m", "\ESC[95m", "\ESC[33m", "\ESC[96m", "\ESC[90m", "\ESC[37m"]

comprobarAccion :: [Char] -> Bool
comprobarAccion letras
   |letras == "flag" = True
   |otherwise = False


{- 
   ** MENUS y temas de IO **

-}
-- CAMBIAR DE SITIO Y ESCRIBIRLA
encontrarMina :: TableroFront -> Bool
encontrarMina tablero = elem True (map (elem (Desc(Mina))) tablero)

-- ¿eliminar la doble copia?

bucleJuego :: TableroFront  -> IO()
bucleJuego tablero = do
   accion       <- bucleAccion
   n            <- fmap (read :: String -> Int) (bucleCoordenada (length tablero -2))
   m            <- fmap (read :: String -> Int) (bucleCoordenadaDos (length (tablero!!0) -2))
   let tablero2     = accionJuego tablero (n,m) accion
   let tableroNuevo = limpiarTableroFeliz tablero2
   if encontrarMina tableroNuevo
   then do
      let tableroPerdedor = descubrirTodo tableroNuevo
      putStrLn ("Has perdido, ¿Quieres volver a jugar? (s/n)")
      mapM_ putStrLn (dibujarTablero tableroPerdedor)
   else mapM_ putStrLn (dibujarTablero tableroNuevo) >> bucleJuego tableroNuevo

-- anadir bandera y descubri
accionJuego :: TableroFront -> (Int,Int) -> String -> TableroFront
accionJuego tablero (n,m) "b" = bandera   0 (n,m) tablero 
accionJuego tablero (n,m) "d" = descubrir 0 (n,m) tablero


bucleAccion :: IO String
bucleAccion = do
   putStrLn "¿Que accion quieres hacer? (bandera/b) o (descubrir/d)"
   accion <- getLine
   if accion == "bandera" || accion == "b" || accion == "descubrir" || accion == "d"
   then return accion
   else putStrLn "No es una accion correcta" >> bucleAccion

bucleCoordenada :: Int -> IO String
bucleCoordenada longitud= do
   putStrLn ("Filas: elige una primera coordenada entre 1 y " ++ (show longitud))
   primera <- getLine
   if elem primera [show x | x <- [1..longitud]]
   then return primera
   else putStrLn "No es una coordenada correcta" >> bucleCoordenada longitud

bucleCoordenadaDos :: Int -> IO String
bucleCoordenadaDos longDos = do
   putStrLn ("Columnas: elige una segunda coordenada entre 1 y " ++ (show longDos))
   segunda <- getLine
   if elem segunda [show x | x <- [1..longDos]]
   then return segunda
   else putStrLn "No es una coordenada correcta" >> bucleCoordenadaDos longDos


{- ** ENDGAME ** -}
