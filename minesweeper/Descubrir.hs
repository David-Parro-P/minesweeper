
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

-- la segunda es el resultado de la funcion buscar ceros que devuelve la posicion

-- Aplicamos una funcion a un elemento concreto del tablero (n,m) que es el segundo input
-- El primer input es un 0 en las llamadas que actua como contador
-- El tercer input es el tablero del que se hará la copia nueva con el cambio

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

-- Hay que hacer la accion en la primera llamada con un int = 0
-- los dos primeros ints son unos contadores de posicion

-- Descubrir es aplicar la funcion descubrirCasilla a la casilla concreta que elige el jugador
descubrir :: Int -> (Int,Int) -> TableroFront -> TableroFront
descubrir contador (n,m) tablero = aplicarConcreto descubrirCasilla contador (n,m) tablero

-- El unico cambio que hace es un Estado NoDesc lo cambia a Descubierto
descubrirCasilla :: Estado -> Estado
descubrirCasilla (NoDesc(Mina))      = (Desc(Mina))
descubrirCasilla (NoDesc(NoMina[x])) = (Desc(NoMina[x]))
descubrirCasilla x                   =  x

-- Funciona igual que descubrir, solo cambia un Estado NoDesc por Flag
-- Para que el jugador tenga la información de que cree que hay una mina ahí

bandera :: Int -> (Int,Int) -> TableroFront -> TableroFront
bandera contador (n,m) tablero = aplicarConcreto banderaCasilla contador (n,m) tablero

banderaCasilla :: Estado -> Estado
banderaCasilla (Desc(x))   = Desc(x)
banderaCasilla (Flag(x))   = NoDesc(x)
banderaCasilla Borde       = Borde
banderaCasilla (NoDesc(x)) = (Flag(x))

-- En las dos siguientes hay que aplicar una función a todo el tablero
-- En Perdedor queremos destapar todo.

descubrirTodoPerdedor :: TableroFront -> TableroFront
descubrirTodoPerdedor tablero = aplicarATodo (introducirCambios lista) tablero
   where lista = [destapar, identidad, destapar, destapar]

-- En Ganador, marca la condición para la victoria, descubrir todas las casillas
-- Y marcar todas las minas
descubrirTodoGanador :: TableroFront -> TableroFront
descubrirTodoGanador tablero = aplicarATodo (introducirCambios lista) tablero
   where lista = [identidad, identidad, minaABandera, destapar]

-- Aplica una funcion a todo el tablero
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

-- Sirve para construir un tablero ganador
minaABandera :: Estado -> Estado
minaABandera (NoDesc(Mina)) = (Flag(Mina))
minaABandera x              = x

-- Muestra el contenido de la casilla
destapar :: Estado -> Estado
destapar (NoDesc(x)) = (Desc(x))
destapar x           = x

-- Condicion de derrota: si encuentra una mina descubierta
-- Marca al programa para mostrar una derrota, ver bucleJuego en Main
encontrarMina :: TableroFront -> Bool
encontrarMina tablero = elem True (map (elem (Desc(Mina))) tablero)
