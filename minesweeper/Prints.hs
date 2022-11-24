module Prints
( dibujarTablero
, dibujarLinea
, dibujarCasilla
, sacarNumDesc
) where
import Datos
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

dibujarCasilla :: Casilla -> [Char]
dibujarCasilla Borde = []
dibujarCasilla Mina = "\ESC[31mx\ESC[0m"
dibujarCasilla (NoMina[x]) = colores!!x ++ show x
   where colores = ["\ESC[37m", "\ESC[34m", "\ESC[92m", "\ESC[91m", "\ESC[35m", "\ESC[95m", "\ESC[33m", "\ESC[96m", "\ESC[90m", "\ESC[37m"]

comprobarAccion :: [Char] -> Bool
comprobarAccion letras
   |letras == "flag" = True
   |otherwise = False


sacarNumDesc :: Estado -> [Char]
sacarNumDesc (Desc (x)) = dibujarCasilla x
