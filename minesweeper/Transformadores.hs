module Transformadores
( modTableroBack
, modFilaBack
, modTableroFront
, modFilaFront
) where
import Datos
-- ** BACK END ** 
modTableroBack :: [[Int]] -> TableroBack
modTableroBack xs = map modFilaBack xs

modFilaBack :: [Int] -> [Casilla]
modFilaBack [] = []
modFilaBack (x:xs)
  |x == 11 = Borde : modFilaBack xs
  |x == 12 = Mina  : modFilaBack xs
  |otherwise = NoMina [x] : modFilaBack xs

-- ** FRONT END **

modTableroFront :: [[Int]] -> TableroFront
modTableroFront xs = map modFilaFront xs

modFilaFront :: [Int] -> [Estado]
modFilaFront [] = []
modFilaFront (x:xs)
  |x == 11 = Aux : modFilaFront xs
  |x == 12 = (NoDesc Mina)  : modFilaFront xs
  |otherwise = (NoDesc (NoMina [x])) : modFilaFront xs
-- Mi idea es unificar los dos porque toda la informacion de back end esta en front
-- ademas solo habria que hacer una funcion de imprimir bonito para sacar el
-- output por pantalla
