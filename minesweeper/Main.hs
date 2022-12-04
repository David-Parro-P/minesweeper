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
import CrearTablero
import System.Random



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

bucleJuego :: TableroFront  -> TableroFront -> IO()
bucleJuego tablero tableroGanador = do
   accion           <- bucleAccion tablero
   n                <- fmap (read :: String -> Int) (bucleCoordenada (length tablero -2))
   m                <- fmap (read :: String -> Int) (bucleCoordenadaDos (length (tablero!!0) -2))
   let tablero2     = accionJuego tablero (n,m) accion
   let tableroNuevo = limpiarTableroFeliz tablero2
   if encontrarMina tableroNuevo
   then do
      let tableroPerdedor = descubrirTodoPerdedor tableroNuevo
      sacarGrafico "derrota.txt"
      dibujarTableroSalida tableroPerdedor >> volverJugar
   else do 
      if tableroNuevo == tableroGanador
      then sacarGrafico "victoria.txt" >> dibujarTableroSalida tableroGanador  >> volverJugar
      else dibujarTableroSalida tableroNuevo   >> bucleJuego tableroNuevo tableroGanador


accionJuego :: TableroFront -> (Int,Int) -> String -> TableroFront
accionJuego tablero (n,m) "b" = bandera   0 (n,m) tablero 
accionJuego tablero (n,m) "d" = descubrir 0 (n,m) tablero


bucleAccion :: TableroFront -> IO String
bucleAccion tablero = do
   putStrLn "¿Que accion quieres hacer? poner bandera (b), descubrir casilla (d) o guardar partida (g)"
   accion <- getLine
   if accion == "g"
   then guardarPartida tablero >> bucleAccion tablero
   else do
      if accion == "bandera" || accion == "b" || accion == "descubrir" || accion == "d"
      then return accion
      else putStrLn "No es una accion correcta" >> bucleAccion tablero 

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
   sacarGrafico "titulo.txt"
   accionM <- bucleAccionMenu
   if accionM == "c" -- bucleAccionMenu protege que estos inputs sean asi 
   then do
      tableroC           <- cargarPartida
      let tableroGanador = descubrirTodoGanador tableroC 
      bucleJuego tableroC tableroGanador
   else do
      tableroN           <- nuevaPartida
      let tableroGanador = descubrirTodoGanador tableroN
      bucleJuego tableroN tableroGanador

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
   putStrLn "Guardado con exito"

cargarPartida :: IO TableroFront
cargarPartida = do
   putStrLn "¿Que partida quieres cargar?"
   rutaCargar   <- getLine
   tableroCarga <- readFile rutaCargar
   return ((read::String->TableroFront) tableroCarga)

nuevaPartida :: IO TableroFront --  CON RANDOM
nuevaPartida = do
   gen <- newStdGen
   (long,nMinas)        <- buclePrimerDescubrir
   putStrLn("¿Que casilla quieres descubrir?")
   n                    <- fmap (read :: String -> Int) (bucleCoordenada (long))
   m                    <- fmap (read :: String -> Int) (bucleCoordenadaDos (long))
   let (matriz,g2)      = crearTablero long nMinas gen (n,m)
   let tableroNuevo     = modTableroFront matriz
   let tableroNuevoDesc = limpiarTableroFeliz(descubrir 0 (n,m) tableroNuevo)
   dibujarTableroSalida tableroNuevoDesc
   return tableroNuevoDesc

buclePrimerDescubrir :: IO (Int,Int)
buclePrimerDescubrir = do
   putStrLn "¿Que dicultad quieres?"
   putStrLn "Facil: 9x9, 10 minas (f)"
   putStrLn "Intermidio: 16x16, 40 minas (i)"
   putStrLn "Dificil: 20x20 , 70 minas (d)"
   accion <- bucleAccionPrimerDescubrir
   if accion == "f"
   then return (9,10)
   else if accion == "i"
        then return (16,40)
        else return (20,70)

bucleAccionPrimerDescubrir :: IO String
bucleAccionPrimerDescubrir = do
   accionPrimera <- getLine
   if accionPrimera == "f" || accionPrimera == "i" || accionPrimera == "d" 
   then return accionPrimera
   else putStrLn "No es una accion correcta, elige f, i o d" >> bucleAccionPrimerDescubrir

debugRandom :: IO ()
debugRandom = do
   gen <- newStdGen
   let (matriz,g2)  = crearTablero 15 60 gen (4,4) -- CAMBIAR 9 por algo general
   let tableroNuevo = modTableroFront matriz
   let tableroDos   = descubrirTodoPerdedor tableroNuevo
   dibujarTableroSalida tableroDos

volverJugar :: IO()
volverJugar = do
   putStrLn "¿Quieres volver a jugar si(s) / no(n)?"
   accion <- getLine
   if accion == "s"
   then bucleMenu
   else do
      if accion == "n"
      then putStrLn "¡Gracias por jugar!"
      else putStrLn "No es una accion correcta" >> volverJugar



{- ** ENDGAME ** -}



