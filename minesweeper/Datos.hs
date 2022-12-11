module Datos 
(Casilla(NoMina, Mina)
,Estado(Flag , Desc, NoDesc, Borde)
,TableroFront 
)where

data Casilla = NoMina [Int] | Mina
   deriving (Eq,Show,Read)
 
data Estado = Flag Casilla | Desc Casilla | NoDesc Casilla | Borde
   deriving (Eq,Show,Read)

type TableroFront = [[Estado]]
