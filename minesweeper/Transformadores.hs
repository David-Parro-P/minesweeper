module Transformadores
( modTableroFront
, modCasillaFront
) where
import Datos

-- Modificamos un tablero numerico a un tablero jugable
-- Se usa para convertir los tableros aleatorios.

modTableroFront :: [[Int]] -> TableroFront
modTableroFront xs = (map.map) modCasillaFront xs

modCasillaFront :: Int -> Estado
modCasillaFront x
  |x == 11   = Borde 
  |x == 12   = (NoDesc Mina)
  |otherwise = (NoDesc (NoMina [x]))
