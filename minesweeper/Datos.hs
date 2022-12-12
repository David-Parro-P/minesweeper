module Datos 
(Casilla(NoMina, Mina)
,Estado(Flag , Desc, NoDesc, Borde)
,TableroFront 
)where
{-
 Tenemos un tipo de datos anidado
 Casilla(El valor verdadero):
 
 Una casilla puede tener una mina o no tenerla, si no la tiene
 Tiene asociado un número que indica cuantas minas tiene en las 8 casillas
 Que lo rodean.
 
 Estado(La información que tiene el jugador):
 
 Las casillas pueden estar descubiertas (Desc), el jugador conoce el contenido
 NoDesc: entonces el jugador no sabe que es la casilla en esa posición
 Borde: es una construcción auxiliar que facilita que en todas las funciones que
 Hay que mirar las 8 casillas que rodean a otra, aplicamos f Borde = Borde
 Flag: sirve para marcar donde el jugador cree que puede haber una mina
 
 Los tableros son [[Estado]] porque se componen de la informacion que conoce
 el jugador.
 
-}
data Casilla = NoMina [Int] | Mina
   deriving (Eq,Show,Read)
 
data Estado = Flag Casilla | Desc Casilla | NoDesc Casilla | Borde
   deriving (Eq,Show,Read)

type TableroFront = [[Estado]]
