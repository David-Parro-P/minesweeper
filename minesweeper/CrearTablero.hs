module CrearTablero
( crearTablero 
, anadirMinas
, anadirMina
, sumarUnos
, sumarUnosMina
, sumaUno
, listaMinas
, busca
, ponerBordes
, bordes
) where
import System.Random

--Función que crea una matriz L*L, con un numero de minas, con una seed
-- y con una casilla alrededor de la cual no se generarán minas.
-- Dentro de la función convierte las casillas de la forma (fila,columna)
-- a un número entre 0 y (L*L-1), siendo L el lado del tablero.
crearTablero::(RandomGen g) => Int-> Int-> g -> (Int,Int) -> ([[Int]],g)
crearTablero lado nminas seed (fil,col)  = (anadirMinas lado (ponerBordes lado tablero0) posminas,seed2)
    where fila             = [0 | x <- [1..lado]]
          tablero0         = [fila | x <- [1..lado]]
          pos              = (fil-1)*(lado) + col-1 
          ceros            = [x+y*lado |y <- [(-2)..2] , x <- [(pos-2)..(pos+2)]]
          (posminas,seed2) = listaMinas lado nminas seed ceros
          
-- Función que recibe un tablero su lado y una lista de casillas
-- y devuelve un tablero en el que en cada posicion se ha añadido una mina,           
anadirMinas:: Int->[[Int]]->[Int]->[[Int]]
anadirMinas lado tablero = foldl (anadirMina lado) tablero 

-- Funcion que recibe un tablero, su lado y una posición.
-- Cambia el numero de esa posición por un 12 que representa una mina 
-- y suma 1 a las 8 casillas de alrededor.          
anadirMina:: Int->[[Int]]->Int->[[Int]]
anadirMina lado tablero pos = arriba ++ [fila1] ++ [fila2]++ [fila3] ++ abajo
    where (arriba,resto)        = splitAt (nFil-1) tablero
          (centro,abajo)        = splitAt 3 resto
          (preFila1,filas)      = splitAt 1 centro
          (preFila2,preFila3)   = splitAt 1 filas
          fila1                 = sumarUnos nCol (concat preFila1)
          fila2                 = sumarUnosMina nCol (concat preFila2)
          fila3                 = sumarUnos nCol (concat preFila3)
          nFil                  = (pos `div` lado) +1
          nCol                  = ((pos+1) `mod` lado)
          
          
-- Función que recibe una posición y una fila del tablero.
-- Suma 1 a esa posicion y a sus contiguos.         
sumarUnos::Int-> [Int]->[Int]
sumarUnos col fila = iz ++ fil ++ dr
    where (iz,resto)   = splitAt (col-1) fila
          (centro, dr) = splitAt 3 resto
          fil          = map sumaUno ( centro)
          
-- Función que recibe una posición y una fila del tablero.
-- Sustituye la posición por un 12 y suma 1 a los contiguos.
sumarUnosMina::Int-> [Int]->[Int]
sumarUnosMina col fila = iz ++ fil ++ dr
    where (iz,resto)   = splitAt (col-1) fila
          (centro, dr) = splitAt 3 resto
          izz          = take 1 centro
          drr          = take 1 (reverse centro)
          fil          = map sumaUno (izz ++ [12] ++ drr)

-- Función que suma 1 a las casillas pero ignora las minas y los bordes.    
sumaUno::Int->Int
sumaUno casilla   
    | casilla==11     = casilla
    | casilla==12     = casilla
    | otherwise       = casilla+1

-- Función que recibe el lado del tablero, un número de minas,
-- una semilla y una lista de posiciones no válidas.
-- Devuelve una lista de posiciones que no contenga ninguna posición repetida
-- o que esté en a lista de posiciones no válidas.
listaMinas::(RandomGen g) => Int -> Int -> g -> [Int]-> ([Int], g)
listaMinas lado 0 seed lista = ([], seed)
listaMinas lado n seed lista   
  | busca r lista = listaMinas lado n seed1 lista
  | otherwise     = (r:nlista , seed2)  
  where (r,seed1)      = randomR (0,tope) seed
        tope           = lado*lado-1
        (nlista,seed2) = listaMinas lado (n-1) seed1 (r:lista) 
          

-- Función que devuelve un true si la posicion está en la lista de posiciones.
busca :: Ord a => a -> [a] -> Bool
busca x xs = not (foldr (&&) True (map (x/=) xs))

  

-- Función que recibe un tablero y su lado.
-- Devuelve un tablero con un borde de onces que representan los bordes.          
ponerBordes::Int -> [[Int]]->[[Int]]
ponerBordes lado tab = fila:(map bordes tab)++[fila]
    where fila    = [11 | x <- [1..(lado+2)]] 

-- Función que añade 11 en los extremos de una fila   
bordes::[Int]->[Int]
bordes fila = 11:fila ++ [11]

