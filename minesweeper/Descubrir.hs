
module Descubrir
( descubrir
, descubrirFila
, descubrirCasilla
, descubrirTodoTest
, descubrirTodo
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


descubrirTodoTest :: TableroFront -> [(Int,Int)] -> TableroFront
descubrirTodoTest tablero [] = tablero
descubrirTodoTest tablero (x:xs) = descubrirTodoTest (descubrir 0 x tablero) xs

descubrirTodo :: TableroFront -> TableroFront
descubrirTodo tablero = descubrirTodoTest tablero casillas
   where casillas = [(x,y) | x <- [1..(length tablero - 2)] , y <- [1..(length (tablero!!0) - 2)]]
