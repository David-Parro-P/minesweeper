module Main
( main
, encontrarMina
, bucleJuego
, accionJuego
, bucleAccion
, bucleCoordenada
, bucleCoordenadaDos
) where

import Datos
import Descubrir
import Banderas
import Ceros
import Prints
import Transformadores



main :: IO ()
main = do
-- lectura y tratamiento
   let ruta = "tests.txt"
   contenido <- readFile ruta
   let tablero = map (map (read::String->Int)) (map words (lines contenido))
   let tableroFrontEnd = modTableroFront tablero
   bucleJuego tableroFrontEnd

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
bucleCoordenada longitud = do
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

guardarPartida :: TableroFront  -> IO()
guardarPartida tablero = do
   putStrLn "¿Con que nombre quieres guardar la partida?"
   rutaGuardado <- getLine
   let contenidoGuardado = show tablero 
   writeFile rutaGuardado contenidoGuardado
-- El espacio del final no molesta con las funciones de lectura
-- words lo soluciona

cargarPartida :: IO TableroFront
cargarPartida = do
   putStrLn "¿Que partida quieres cargar?"
   rutaCargar   <- getLine
   tableroCarga <- readFile rutaCargar
   return ((read::String->TableroFront) tableroCarga)

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs

juntarConComa :: [String] -> String
juntarConComa xs = (foldr (++) "" listaMod) ++ "\n"
   where listaMod   = map (++",") xs