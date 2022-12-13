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

-- Muestra título y calcula el tableroGanador para poder pasarlo como argumento
-- Y no tener que calcularlo en cada movimiento
main :: IO()
main = do
   sacarGrafico "titulo.txt"
   tableroNorC        <- bucleAccionMenu
   let tableroGanador = descubrirTodoGanador tableroNorC
   bucleJuego tableroNorC tableroGanador
   
-- Le pregunta al jugador si quiere nueva partida o cargar partida
bucleAccionMenu :: IO TableroFront
bucleAccionMenu = do
   menuGeneral [("n",nuevaPartida),("c",cargarPartida)]
    ("¿Quieres empezar una nueva partida (n) o cargar una existente (c)?")

-- Carga una partida con la ruta que le pidas
cargarPartida :: IO TableroFront
cargarPartida = do
   putStrLn "¿Que partida quieres cargar?"
   rutaCargar   <- getLine
   tableroCarga <- readFile rutaCargar
   return ((read::String->TableroFront) tableroCarga)

{-
 Función destacada, aunque se parezca mucho a bucleJuego es necesaria, se ocupa 
 De un caso importante, el primer movimiento, solo deja descubrir en la coordenada
 Que elija el jugador y lo diferente es que a crearTablero se le pasa la coordenada
 Para que en esa coordenada NO haya mina y por tanto no se pueda perder en el primer movimiento
-}
nuevaPartida :: IO TableroFront 
nuevaPartida = do
   gen <- newStdGen
   (long,nMinas)        <- buclePrimerDescubrir
   putStrLn("¿Que casilla quieres descubrir?")
   n                    <- fmap (read :: String -> Int) (bucleCoordenada "Filas" (long))
   m                    <- fmap (read :: String -> Int) (bucleCoordenada "Columnas" (long))
   let matriz      = crearTablero long nMinas gen (n,m)
   let tableroNuevo     = modTableroFront matriz
   let tableroNuevoDesc = limpiarTableroFeliz(descubrir 0 (n,m) tableroNuevo)
   dibujarTableroSalida tableroNuevoDesc
   return tableroNuevoDesc

{-
 1. Le pregunta al jugador que queire hacer (descubrir, bandera, guardar)
 2. Si es de las dos primeras elige coordenada
 3. Si ha descubierto una mina, muestra mensaje de derrota
 4. Comprueba si está en estado ganador
 4.A. Si es ganador muestra mensaje de victoria
 4.B. Si es movimiento válido que no gana vuelve a repetir el bucle.
-}
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


-- Le permite al jugador elegir lo que quiere hacer, descubrir, poner una bandera o guardar partida.
bucleAccion :: TableroFront -> IO String
bucleAccion tablero = do
   menuGeneral [("b",return "b"),("d",return "d")
    ,("g", (guardarPartida tablero >> bucleAccion tablero))]
     ("¿Que accion quieres hacer? poner bandera (b), descubrir casilla (d) o guardar partida (g)")

-- Si ha elegido bandera o descubrir lo ejecuta en la casilla elegida.
accionJuego :: TableroFront -> (Int,Int) -> String -> TableroFront
accionJuego tablero (n,m) "b" = bandera   0 (n,m) tablero 
accionJuego tablero (n,m) "d" = descubrir 0 (n,m) tablero

-- Le pregunta al jugador que casilla quiere descubrir/poner bandera, si no es una posición válida
-- Vuelve a preguntar 
bucleCoordenada :: String -> Int -> IO String
bucleCoordenada filOCol longitud = do
   putStrLn (filOCol ++ ": elige una primera coordenada entre 1 y " ++ (show longitud))
   primera <- getLine
   if elem primera [show x | x <- [1..longitud]]
   then return primera
   else putStrLn "No es una coordenada correcta" >> bucleCoordenada filOCol longitud

-- Guarda la la partida actual en un fichero que posteriormente se puede cargar para retormar.
guardarPartida :: TableroFront  -> IO()
guardarPartida tablero = do
   putStrLn "¿Con que nombre quieres guardar la partida?"
   rutaGuardado          <- getLine
   let contenidoGuardado = show tablero 
   writeFile rutaGuardado contenidoGuardado
   putStrLn "Guardado con exito"

-- Le permite al jugador elegir la dificultad que quiere para el juego
buclePrimerDescubrir :: IO (Int,Int)
buclePrimerDescubrir = do
   menuGeneral [("f",return (9,10)),("i",return (16,40)),("d",return (20,70))]
    ("¿Que dicultad quieres?\n" ++ "Facil: 9x9, 10 minas (f)\n" ++ 
     "Intermedio: 16x16, 40 minas (i)\n" ++ "Dificil: 20x20 , 70 minas (d)")

-- Da opción de Si o No (responder (s) o (n), si responde n imprime un agradecimiento
volverJugar :: IO()
volverJugar = do
   menuGeneral [("s", main),("n",putStrLn "¡Gracias por jugar!")]
    ("¿Quieres volver a jugar si(s) / no(n)?")

{-
 Recibe
 [(String,(IO a))]: el primer elemento del par es la opcion correspondiente
 , por ejemplo "n" para No "s" para Si
 El segundo elemento del par es una función que ejecuta lo que deberia pasar
 segun la opcion que elige el jugador
 
 El otro String es la pregunta que se le hace al jugador
 
 Si ningún input esta entre las posibles respuestas válidas,
 lo indica y repite la pregunta para evitar excepciones
-}
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

-- Auxiliar que localiza la respuesta que tiene que ejecutar
-- Cuando llega a esta ya se sabe que la respuesta es válida
ejecutarRespuesta :: [(String, IO a)] -> String -> IO a
ejecutarRespuesta (x:xs) respuesta
   |fst(x) == respuesta  = snd(x)
   |otherwise            = ejecutarRespuesta xs respuesta
