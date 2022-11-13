-- "C:/Users/David/Desktop/progDeclarativa/Haskell/minesweeper-main/test_funciones.hs"

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
   let ruta = "tests.txt"
   contenido <- readFile ruta
   let tablero = map (map (read::String->Int)) (map words (lines contenido))
   let tableroFrontEnd = modTableroFront tablero
   -- juego 
   bucleJuego tableroFrontEnd

{- 
   Corregir: deja meter un string de un caracter como coordenada
   No puede meter dos ints como coordenada tipo 10 
   me gustaria otro salto de linea por claridad
-}

bucleJuego :: TableroFront  -> IO()
bucleJuego tablero = do
   accion       <- bucleAccion
   n            <- fmap (read :: String -> Int) bucleCoordenada
   m            <- fmap (read :: String -> Int) bucleCoordenadaDos
   let tableroNuevo = descubrir 0 (n,m) tablero
   if encontrarMina tableroNuevo
   then putStrLn "Espabila"
   else mapM_ putStrLn (dibujarTablero tablero) >> bucleJuego tableroNuevo

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


{-               **TRABAJAR DESCUBRIR**
-- Anadir un si la coordenada no es correcta o len incorrecta volver a introducir
descubrir :: (Int, Int) -> Tablero -> Tablero -6- Cambiar bool por un tablero 
descubrir (n,m) tablero_back
   |(tablero_back!!n)!!m ==  Mina = endGame -- Muestra tablero resuelto y mensaje
   |(tablero_back!!n)!!m == NoMina[x] -- cambia estado tablero_front

-}

-- hay que hacer la accion en la priemra llamada con un int = 0
-- los dos priemros ints son unos contadores de posicion
-- la segunda es el resultado de la function buscar ceros que devuelve la posicion

-- rematar esta 

encontrarMina :: TableroFront -> Bool
encontrarMina tablero = elem True (map (elem (Desc(Mina))) tablero)

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
-- map ((flipTresMedioDcha descubrirCeros) (0,0) tablero) (buscarCerosTablero (0,0) tablero)

descubrirCeros :: (Int,Int) -> (Int,Int) -> TableroFront -> TableroFront
descubrirCeros (contX,contY) (n,m) [] = []
descubrirCeros (contX,contY) (n,m) (x:xs)
   |(contX, contY) == (n-1, m-1) = descubrirCeros (contX,contY+1) (n,m) ((descubrirFila (n-1,m-1) x) : xs)
   |(contX, contY) == (n-1, m)   = descubrirCeros (contX,contY+1) (n,m) ((descubrirFila (n-1,m) x) : xs)
   |(contX, contY) == (n-1, m+1) = (descubrirFila (n-1,m+1) x) : (descubrirCeros (n,m-1) (n,m) xs)
   |(contX, contY) == (n, m-1)   = descubrirCeros (contX,contY+1) (n,m) ((descubrirFila (n,m-1) x) : xs)
   |(contX, contY) == (n, m)     = descubrirCeros (contX,contY+1) (n,m) ((descubrirFila (n,m) x) : xs)
   |(contX, contY) == (n, m+1)   = (descubrirFila (n,m+1) x) : (descubrirCeros (n+1,m-1) (n,m) xs)
   |(contX, contY) == (n+1, m-1) = descubrirCeros (contX,contY+1) (n,m) ((descubrirFila (n+1,m-1) x) : xs)
   |(contX, contY) == (n+1, m)   = descubrirCeros (contX,contY+1) (n,m) ((descubrirFila (n+1,m) x) : xs)
   |(contX, contY) == (n+1, m+1) = (descubrirFila (n+1,m+1) x) : xs
   |fst((contX, contY)) < n-1    = descubrirCeros (contX+1,contY) (n,m) (x:xs)
   |fst((contX, contY)) == n-1   = descubrirCeros (contX,contY+1) (n,m) (x:xs)

descubrir :: Int -> (Int,Int) -> TableroFront -> TableroFront
descubrir contador (n,m) [] = []
descubrir contador (n,m) (x:xs)
   |contador == n = (descubrirFila (n,m) x) : xs 
   |otherwise = x : (descubrir (contador+1) (n,m) xs)

-- con las minas todavia no he definido, ni decidido, que pasa 
descubrirFila :: (Int,Int) -> [Estado] -> [Estado] -- Cambiar bool por un tablero 
descubrirFila (n,m) fila = primera ++ [descubrirCasilla posicion] ++ quitarHead(segunda)
   where primera = fst(splitAt m (fila))
         segunda = snd(splitAt m (fila))
         posicion = (fila)!!m

descubrirCasilla :: Estado -> Estado
descubrirCasilla (Desc(x)) = Desc(x)
descubrirCasilla Flag = Flag
descubrirCasilla Aux = Aux
descubrirCasilla (NoDesc(NoMina[x]))
   |x == 0 = Desc(NoMina[x])
   |otherwise = (Desc(NoMina[x]))

quitarHead :: [a] -> [a]
quitarHead [] = []
quitarHead (x:xs) = xs
