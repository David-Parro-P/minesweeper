module Ceros
( buscarCerosTablero
, limpiarTableroFeliz
, limpiarCerosFront
, limpiarCerosTablero
, limpiarCeros
, flipTresMedioDcha
, repetirN
, listIncidencia
) where
import Datos
import Descubrir

buscarCerosTablero :: (Int,Int) -> TableroFront -> [(Int,Int)]
buscarCerosTablero _ [[]] = []
buscarCerosTablero (n,m) tablero
   |tablero!!0 == [] = []
   |a == length(tablero)                 = []
   |b >= length(tablero!!0)              = (buscarCerosTablero (a+1,0) tablero)

   |(tablero!!a)!!b == (Desc(NoMina[0])) = (a,b) : (buscarCerosTablero (a,b+1) tablero)
   |otherwise = buscarCerosTablero (a,b+1) tablero
   where a = fst (n,m)
         b = snd (n,m)

flipTresMedioDcha :: (a -> b -> c -> d) -> a -> c -> b -> d
flipTresMedioDcha f x z y = f x y z

-- la funcion que busca ceros despues de cada loop es pasar 
-- map (flipTresMedioDcha descubrirCeros (0,0) tablero2) (buscarCerosTablero (0,0) tablero2)

limpiarTableroFeliz :: TableroFront -> TableroFront
limpiarTableroFeliz  tablero = repetirN limpiarCerosFront tablero (length tablero)

limpiarCerosFront :: TableroFront -> TableroFront
limpiarCerosFront tablero   =  f listaDeIncidencias
   where listaDeIncidencias = map listIncidencia (buscarCerosTablero (0,0) tablero)
         f                  = limpiarCerosTablero tablero

repetirN :: (a -> a) -> a -> Int -> a
repetirN f x n  = iterate f x !! n


limpiarCerosTablero :: TableroFront -> [[(Int,Int)]] -> TableroFront
limpiarCerosTablero tablero []   = tablero
limpiarCerosTablero tablero (x:xs) = limpiarCerosTablero (limpiarCeros tablero x) xs 


limpiarCeros :: TableroFront -> [(Int,Int)] -> TableroFront
limpiarCeros tablero []     = tablero
limpiarCeros tablero (x:xs) = limpiarCeros (descubrir 0 x tablero) xs

listIncidencia :: (Int,Int) -> [(Int,Int)]
listIncidencia (n,m) = [(n+x,m+y) | x <- [-1,0,1], y <- [-1,0,1]]  
