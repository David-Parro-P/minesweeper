module Main
( main
, bucleAccionMenu
, cargarPartida
, nuevaPartida
, guardarPartida
, buclePrimerDescubrir
, volverJugar
, menuGeneral
, ejecutarRespuesta
, bucleJuego
, accionJuego
, bucleAccion
, bucleCoordenada
) where

import Datos
import Descubrir
import Ceros
import Prints
import Transformadores
import CrearTablero
import System.Random

main :: IO()
main = do
   sacarGrafico "titulo.txt"
   tableroNorC        <- bucleAccionMenu
   let tableroGanador = descubrirTodoGanador tableroNorC
   bucleJuego tableroNorC tableroGanador
   
-- solucionado
bucleAccionMenu :: IO TableroFront
bucleAccionMenu = do
   menuGeneral [("n",nuevaPartida),("c",cargarPartida)]
    ("¿Quieres empezar una nueva partida (n) o cargar una existente (c)?")

-- solucionado
cargarPartida :: IO TableroFront
cargarPartida = do
   putStrLn "¿Que partida quieres cargar?"
   rutaCargar   <- getLine
   tableroCarga <- readFile rutaCargar
   return ((read::String->TableroFront) tableroCarga)

-- solucionado
nuevaPartida :: IO TableroFront 
nuevaPartida = do
   gen <- newStdGen
   (long,nMinas)        <- buclePrimerDescubrir
   putStrLn("¿Que casilla quieres descubrir?")
   n                    <- fmap (read :: String -> Int) (bucleCoordenada "Filas" (long))
   m                    <- fmap (read :: String -> Int) (bucleCoordenada "Columnas" (long))
   let (matriz,g2)      = crearTablero long nMinas gen (n,m)
   let tableroNuevo     = modTableroFront matriz
   let tableroNuevoDesc = limpiarTableroFeliz(descubrir 0 (n,m) tableroNuevo)
   dibujarTableroSalida tableroNuevoDesc
   return tableroNuevoDesc

bucleJuego :: TableroFront  -> TableroFront -> IO()
bucleJuego tablero tableroGanador = do
   accion           <- bucleAccion tablero
   n                <- fmap (read :: String -> Int) (bucleCoordenada "Filas" (length tablero -2))
   m                <- fmap (read :: String -> Int) (bucleCoordenada "Columnas" (length (tablero!!0) -2))
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

-- solucionado
bucleAccion :: TableroFront -> IO String
bucleAccion tablero = do
   menuGeneral [("b",return "b"),("d",return "d")
    ,("g", (guardarPartida tablero >> bucleAccion tablero))]
     ("¿Que accion quieres hacer? poner bandera (b), descubrir casilla (d) o guardar partida (g)")

-- solucionado 
bucleCoordenada :: String -> Int -> IO String
bucleCoordenada filOCol longitud = do
   putStrLn (filOCol ++ ": elige una primera coordenada entre 1 y " ++ (show longitud))
   primera <- getLine
   if elem primera [show x | x <- [1..longitud]]
   then return primera
   else putStrLn "No es una coordenada correcta" >> bucleCoordenada filOCol longitud

-- solucionado
guardarPartida :: TableroFront  -> IO()
guardarPartida tablero = do
   putStrLn "¿Con que nombre quieres guardar la partida?"
   rutaGuardado          <- getLine
   let contenidoGuardado = show tablero 
   writeFile rutaGuardado contenidoGuardado
   putStrLn "Guardado con exito"

-- solucionado
buclePrimerDescubrir :: IO (Int,Int)
buclePrimerDescubrir = do
   menuGeneral [("f",return (9,10)),("i",return (16,40)),("d",return (20,70))]
    ("¿Que dicultad quieres?\n" ++ "Facil: 9x9, 10 minas (f)\n" ++ 
     "Intermedio: 16x16, 40 minas (i)\n" ++ "Dificil: 20x20 , 70 minas (d)")

-- solucionado
volverJugar :: IO()
volverJugar = do
   menuGeneral [("s", main),("n",putStrLn "¡Gracias por jugar!")]
    ("¿Quieres volver a jugar si(s) / no(n)?")

-- solucionado
menuGeneral :: [(String,(IO a))] -> String -> IO a
menuGeneral lista pregunta = do
   let separados = unzip lista
   let inputsVal = fst separados
   let funciones = snd separados
   putStrLn pregunta
   respuesta <- getLine
   if elem respuesta inputsVal
   then ejecutarRespuesta lista respuesta
   else putStrLn "No es una respuesta valida" >> menuGeneral lista pregunta

-- solucionado
ejecutarRespuesta :: [(String, IO a)] -> String -> IO a
ejecutarRespuesta (x:xs) respuesta
   |fst(x) == respuesta  = snd(x)
   |otherwise            = ejecutarRespuesta xs respuesta
