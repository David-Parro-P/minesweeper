module Prints
( dibujarTablero
, dibujarTableroSalida
, dibujarLinea
, dibujarCasilla
, sacarNumDesc
, sacarGrafico
) where
import Datos


dibujarTablero :: TableroFront -> [[Char]]
dibujarTablero tablero = map dibujarLinea tablero

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

dibujarCasilla :: Casilla -> [Char]
dibujarCasilla Mina        = "\ESC[31mx\ESC[0m  "
dibujarCasilla (NoMina[x]) = colores!!x ++ show x ++ "\ESC[0m" ++ "  "
   where colores = ["\ESC[37m", "\ESC[34m", "\ESC[92m", "\ESC[91m", "\ESC[35m", "\ESC[95m", "\ESC[33m", "\ESC[96m", "\ESC[90m", "\ESC[37m"]

sacarNumDesc :: Estado -> [Char]
sacarNumDesc (Desc (x)) = dibujarCasilla x

dibujarTableroSalida :: TableroFront -> IO()
dibujarTableroSalida tablero = do
   mapM_ putStrLn (dibujarTablero tablero)

sacarGrafico :: [Char] -> IO()
sacarGrafico ruta = do
   contenidoTit      <- readFile ruta
   let contenidoTitL = (lines contenidoTit)
   mapM_ putStrLn contenidoTitL
