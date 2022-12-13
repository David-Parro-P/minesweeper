module Ceros
(nuevoDescubrirLista
,nuevoDescubrir
) where
import Datos
import Descubrir



nuevoDescubrirLista :: TableroFront -> [(Int,Int)] -> TableroFront 
nuevoDescubrirLista tablero [] = tablero
nuevoDescubrirLista tablero (x:xs) = nuevoDescubrirLista (nuevoDescubrir tablero x) xs

nuevoDescubrir :: TableroFront -> (Int,Int) -> TableroFront
nuevoDescubrir tablero (fil,col)
   |(tablero!!fil)!!col == (NoDesc(NoMina[0])) = nuevoDescubrirLista (descubrir 0 (fil,col) tablero) lista
   |otherwise                 = descubrir 0 (fil,col) tablero
   where lista = [(fil,col-1)]++[(fil,col+1)]++[(fil+y, col+x)| x <- [-1,0,1],y <- [-1,1]] 
