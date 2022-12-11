
module Descubrir
(aplicarConcreto
, aplicarConcretoFila
, descubrir
, descubrirCasilla
, bandera
, banderaCasilla
, descubrirTodoPerdedor
, descubrirTodoGanador
, aplicarATodo
, introducirCambios
, identidad
, minaABandera
, destapar
, encontrarMina
) where

import Datos
-- Hay que hacer la accion en la priemra llamada con un int = 0
-- los dos primeros ints son unos contadores de posicion
-- la segunda es el resultado de la funcion buscar ceros que devuelve la posicion


aplicarConcreto :: (Estado -> Estado) -> Int -> (Int,Int) -> TableroFront -> TableroFront
aplicarConcreto f contador (n,m) [] = []
aplicarConcreto f contador (n,m) (x:xs)
   |contador == n = (aplicarConcretoFila f (n,m) x) : xs 
   |otherwise     = x : (aplicarConcreto f (contador+1) (n,m) xs)

aplicarConcretoFila :: (Estado -> Estado) -> (Int,Int) -> [Estado] -> [Estado] 
aplicarConcretoFila f (n,m) fila = primera ++ [f posicion] ++ tail(segunda)
   where primera  = fst(splitAt m (fila))
         segunda  = snd(splitAt m (fila))
         posicion = (fila)!!m

descubrir :: Int -> (Int,Int) -> TableroFront -> TableroFront
descubrir contador (n,m) tablero = aplicarConcreto descubrirCasilla contador (n,m) tablero

descubrirCasilla :: Estado -> Estado
descubrirCasilla (NoDesc(Mina))      = (Desc(Mina))
descubrirCasilla (NoDesc(NoMina[x])) = (Desc(NoMina[x]))
descubrirCasilla x                   =  x

bandera :: Int -> (Int,Int) -> TableroFront -> TableroFront
bandera contador (n,m) tablero = aplicarConcreto banderaCasilla contador (n,m) tablero

banderaCasilla :: Estado -> Estado
banderaCasilla (Desc(x))   = Desc(x)
banderaCasilla (Flag(x))   = NoDesc(x)
banderaCasilla Borde       = Borde
banderaCasilla (NoDesc(x)) = (Flag(x))

descubrirTodoPerdedor :: TableroFront -> TableroFront
descubrirTodoPerdedor tablero = aplicarATodo (introducirCambios lista) tablero
   where lista = [destapar, identidad, destapar, destapar]
   
descubrirTodoGanador :: TableroFront -> TableroFront
descubrirTodoGanador tablero = aplicarATodo (introducirCambios lista) tablero
   where lista = [identidad, identidad, minaABandera, destapar]

aplicarATodo :: (Estado -> Estado) -> TableroFront -> TableroFront
aplicarATodo f tablero = (map.map) f tablero

-- Estructura, una lista de 4 funciones, cada una trabaja un Estado
-- L!!0 decide que pasa con Flag
-- L!!1 decide que pasa con Desc
-- L!!2 decide que pasa con NoDesc(Mina)
-- L!!3 decide que pasa con NoDesc(NoMina[x])

introducirCambios :: [(Estado -> Estado)] -> Estado -> Estado
introducirCambios listaF Borde               =  Borde
introducirCambios listaF (Flag(x))           = (listaF!!0) (Flag(x))
introducirCambios listaF (Desc(x))           = (listaF!!1) (Desc(x))
introducirCambios listaF (NoDesc(Mina))      = (listaF!!2) (NoDesc(Mina))
introducirCambios listaF (NoDesc(NoMina[x])) = (listaF!!3) (NoDesc(NoMina[x]))

identidad :: Estado -> Estado
identidad x = x

minaABandera :: Estado -> Estado
minaABandera (NoDesc(Mina)) = (Flag(Mina))
minaABandera x              = x

destapar :: Estado -> Estado
destapar (NoDesc(x)) = (Desc(x))
destapar x           = x

encontrarMina :: TableroFront -> Bool
encontrarMina tablero = elem True (map (elem (Desc(Mina))) tablero)
