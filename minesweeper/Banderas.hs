{- ** FLAG ** -}
module Banderas
( bandera
, banderaFila
, banderaCasilla
) where
import Datos
import Descubrir

bandera :: Int -> (Int,Int) -> TableroFront -> TableroFront
bandera contador (n,m) [] = []
bandera contador (n,m) (x:xs)
   |contador == n = (banderaFila (n,m) x) : xs 
   |otherwise = x : (bandera (contador+1) (n,m) xs)

banderaFila :: (Int,Int) -> [Estado] -> [Estado] 
banderaFila (n,m) fila = primera ++ [banderaCasilla posicion] ++ tail(segunda)
   where primera = fst(splitAt m (fila))
         segunda = snd(splitAt m (fila))
         posicion = (fila)!!m

banderaCasilla :: Estado -> Estado
banderaCasilla (Desc(x)) = Desc(x)
banderaCasilla (Flag(x)) = NoDesc(x)
banderaCasilla Aux = Aux
banderaCasilla (NoDesc(x)) = (Flag(x))