module Prints
( dibujarTablero
, dibujarTableroSalida
, dibujarLinea
, dibujarCasilla
, sacarNumDesc
, sacarGrafico
) where
import Datos

-- Construye la string para sacar el tablero por pantalla
-- Cada elemento es una linea

dibujarTablero :: TableroFront -> [[Char]]
dibujarTablero tablero = map dibujarLinea tablero

-- Construye la linea para el tablero, \ESC[XXm son colores
-- ! son banderas
-- * son no descubiertos
-- x son minas
-- Los numeros corresponden a NoMina[x] a la x

dibujarLinea :: [Estado] -> [Char]
dibujarLinea [] = ""
dibujarLinea (x:xs)
   |x == Borde             = "" ++ dibujarLinea xs
   |x == (Flag(Mina))      = "\ESC[31m!\ESC[0m  " ++ dibujarLinea xs
   |elem x listNoDescFlag  = "\ESC[31m!\ESC[0m  " ++ dibujarLinea xs
   |x == NoDesc(Mina)      = "\ESC[0m*\ESC[0m  " ++ dibujarLinea xs
   |elem x listNoDesc      = "\ESC[0m*\ESC[0m  " ++ dibujarLinea xs
   |otherwise = sacarNumDesc x ++ dibujarLinea xs
  where listNoDesc     = [NoDesc(NoMina[i]) | i <- [0..9]]
        listNoDescFlag = [Flag(NoMina[i]) | i <- [0..9]]

-- Cuando queremos dibujar un NoMina[x], nos interesa sacar la x
-- Este encuentra la x y se la añade con el color correcto
-- Si es mina ponemos una x

dibujarCasilla :: Casilla -> [Char]
dibujarCasilla Mina        = "\ESC[31mx\ESC[0m  "
dibujarCasilla (NoMina[x]) = colores!!x ++ show x ++ "\ESC[0m" ++ "  "
   where colores = ["\ESC[37m", "\ESC[34m", "\ESC[92m", "\ESC[91m", "\ESC[35m", "\ESC[95m", "\ESC[33m", "\ESC[96m", "\ESC[90m", "\ESC[37m"]

-- Funcion auxiliar del anterior para poder dibujar el numero

sacarNumDesc :: Estado -> [Char]
sacarNumDesc (Desc (x)) = dibujarCasilla x


-- Saca el tablero por pantalla, con la string ya hecha con los colores

dibujarTableroSalida :: TableroFront -> IO()
dibujarTableroSalida tablero = do
   mapM_ putStrLn (dibujarTablero tablero)

-- Recibe una ruta e imprime el contenido del archivo por pantalla

sacarGrafico :: [Char] -> IO()
sacarGrafico ruta = do
   contenidoTit      <- readFile ruta
   let contenidoTitL = (lines contenidoTit)
   mapM_ putStrLn contenidoTitL
