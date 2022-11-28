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
   -- bucleJuego tableroFrontEnd
   -- writeFile "testRead.txt" (show tableroFrontEnd)
   bucleMenu

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
      mapM_ putStrLn (dibujarTablero tableroPerdedor) >> volverJugar
   else mapM_ putStrLn (dibujarTablero tableroNuevo) >> bucleJuego tableroNuevo


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

bucleMenu :: IO()
bucleMenu = do
   accionM <- bucleAccionMenu
   if accionM == "c"
   then do
      tableroC  <- cargarPartida
      bucleJuego tableroC
   else do
      tableroN <- nuevaPartida
      bucleJuego tableroN

bucleAccionMenu :: IO String
bucleAccionMenu = do
   putStrLn "¿Quieres empezar una nueva partida (n) o cargar una existente (c)?"
   accionMenu <- getLine
   if accionMenu == "n" || accionMenu == "c" 
   then return accionMenu
   else putStrLn "No es una accion correcta" >> bucleAccionMenu

guardarPartida :: TableroFront  -> IO()
guardarPartida tablero = do
   putStrLn "¿Con que nombre quieres guardar la partida?"
   rutaGuardado <- getLine
   let contenidoGuardado = show tablero 
   writeFile rutaGuardado contenidoGuardado

cargarPartida :: IO TableroFront
cargarPartida = do
   putStrLn "¿Que partida quieres cargar?"
   rutaCargar   <- getLine
   tableroCarga <- readFile rutaCargar
   return ((read::String->TableroFront) tableroCarga)

nuevaPartida :: IO TableroFront -- PLACE HOLDER CAMBIAR POR FUNCION NUEVA CON RANDOM
nuevaPartida = do
   putStrLn "¿Que partida quieres cargar?"
   rutaCargar   <- getLine
   tableroCarga <- readFile rutaCargar
   return ((read::String->TableroFront) tableroCarga)

volverJugar :: IO()
volverJugar = do
   putStrLn "¿Quieres volver a jugar si(s)/no(n)?"
   accion <- getLine
   if accion == "s"
   then bucleMenu
   else do
      if accion == "n"
      then putStrLn "¡Gracias por jugar!"
      else putStrLn "No es una accion correcta" >> volverJugar 
{- ** ENDGAME ** -}






{- Seguir el esquema del folio de menu
   intenta que sea medio pura sin ifs
-}


