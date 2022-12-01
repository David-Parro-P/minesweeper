
module Descubrir
( descubrir
, descubrirFila
, descubrirCasilla
, descubrirTodoPerdedor
, descubrirCasillaPerdedor
, descubrirTodoGanador
, descubrirCasillaGanador
, encontrarMina
) where

import Datos
-- hay que hacer la accion en la priemra llamada con un int = 0
-- los dos primeros ints son unos contadores de posicion
-- la segunda es el resultado de la funcion buscar ceros que devuelve la posicion

descubrir :: Int -> (Int,Int) -> TableroFront -> TableroFront
descubrir contador (n,m) [] = []
descubrir contador (n,m) (x:xs)
   |contador == n = (descubrirFila (n,m) x) : xs 
   |otherwise = x : (descubrir (contador+1) (n,m) xs)

descubrirFila :: (Int,Int) -> [Estado] -> [Estado] 
descubrirFila (n,m) fila = primera ++ [descubrirCasilla posicion] ++ tail(segunda)
   where primera = fst(splitAt m (fila))
         segunda = snd(splitAt m (fila))
         posicion = (fila)!!m

descubrirCasilla :: Estado -> Estado
descubrirCasilla (Desc(x)) = Desc(x)
descubrirCasilla (Flag(x)) = Flag(x)
descubrirCasilla Aux = Aux
descubrirCasilla (NoDesc(Mina)) = (Desc(Mina))
descubrirCasilla (NoDesc(NoMina[x]))
   |x == 0 = Desc(NoMina[x])
   |otherwise = (Desc(NoMina[x]))

-- Para ensenar todo endGame
descubrirTodoPerdedor :: TableroFront -> TableroFront
descubrirTodoPerdedor tablero = (map.map) descubrirCasillaPerdedor tablero
-- Este destapa banderas para el endgame perdedor

descubrirCasillaPerdedor :: Estado -> Estado
descubrirCasillaPerdedor (Desc(x))           = Desc(x)
descubrirCasillaPerdedor (Flag(x))           = Desc(x)
descubrirCasillaPerdedor Aux                 = Aux
descubrirCasillaPerdedor (NoDesc(Mina))      = (Desc(Mina))
descubrirCasillaPerdedor (NoDesc(NoMina[x])) = Desc(NoMina[x])

-- Para ensenar todo endGame
descubrirTodoGanador :: TableroFront -> TableroFront
descubrirTodoGanador tablero = (map.map) descubrirCasillaGanador tablero
-- Este destapa banderas para el endgame perdedor

descubrirCasillaGanador :: Estado -> Estado
descubrirCasillaGanador (Desc(x))           = Desc(x)
descubrirCasillaGanador (Flag(x))           = Flag(x)
descubrirCasillaGanador Aux                 = Aux
descubrirCasillaGanador (NoDesc(Mina))      = (Flag(Mina))
descubrirCasillaGanador (NoDesc(NoMina[x])) = Desc(NoMina[x])

encontrarMina :: TableroFront -> Bool
encontrarMina tablero = elem True (map (elem (Desc(Mina))) tablero)
