module Datos 
(Casilla(NoMina, Mina, Borde)
,Estado(Flag , Desc, NoDesc, Aux)
,TableroFront
,TableroBack 
)where

data Casilla = NoMina [Int] | Mina  | Borde
   deriving (Eq,Show,Read)

type TableroBack = [[Casilla]]
type TableroFront = [[Estado]]

data Estado = Flag Casilla | Desc Casilla | NoDesc Casilla | Aux -- Conseguir sustituir aux
   deriving (Eq,Show,Read)                              -- aqui o eliminarlos de front
