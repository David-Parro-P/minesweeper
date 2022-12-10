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


crearTablero::(RandomGen g) => Int-> Int-> g -> (Int,Int) -> ([[Int]],g)
crearTablero lado nminas seed (fil,col)  = (anadirMinas lado (ponerBordes lado tablero0) posminas,seed2)
    where fila             = [0 | x <- [1..lado]]
          tablero0         = [fila | x <- [1..lado]]
          pos              = (fil-1)*(lado) + col-1
          ceros            = [x+y*lado |y <- [(-2)..2] , x <- [(pos-2)..(pos+2)]]
          (posminas,seed2) = listaMinas lado nminas seed ceros
          
          
anadirMinas:: Int->[[Int]]->[Int]->[[Int]]
anadirMinas lado tablero = foldl (anadirMina lado) tablero 

         
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
          
          
          
sumarUnos::Int-> [Int]->[Int]
sumarUnos b fila = iz ++ fil ++ dr
    where (iz,resto)   = splitAt (b-1) fila
          (centro, dr) = splitAt 3 resto
          fil          = map sumaUno ( centro)

sumarUnosMina::Int-> [Int]->[Int]
sumarUnosMina col fila = iz ++ fil ++ dr
    where (iz,resto)   = splitAt (col-1) fila
          (centro, dr) = splitAt 3 resto
          izz          = take 1 centro
          drr          = take 1 (reverse centro)
          fil          = map sumaUno (izz ++ [12] ++ drr)
    
sumaUno::Int->Int
sumaUno casilla   
    | casilla==11     = casilla
    | casilla==12     = casilla
    | otherwise       = casilla+1

listaMinas::(RandomGen g) => Int -> Int -> g -> [Int]-> ([Int], g)
listaMinas lado 0 seed lista = ([], seed)
listaMinas lado n seed lista   
  | busca r lista = listaMinas lado n seed1 lista
  | otherwise     = (r:nlista , seed2)  
  where (r,seed1)      = randomR (0,tope) seed
        tope           = lado*lado-1
        (nlista,seed2) = listaMinas lado (n-1) seed1 (r:lista) 
          


busca :: Ord a => a -> [a] -> Bool
busca x xs = not (foldr (&&) True (map (x/=) xs))

  

          
ponerBordes::Int -> [[Int]]->[[Int]]
ponerBordes lado tab = fila:(map bordes tab)++[fila]
    where fila    = [11 | x <- [1..(lado+2)]] 
    
bordes::[Int]->[Int]
bordes fila = 11:fila ++ [11]

