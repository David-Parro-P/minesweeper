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


crearTablero::(RandomGen g) => Int-> Int-> g -> [Int]-> ([[Int]],g)
crearTablero l nminas g desc  = (anadirMinas l posminas (ponerBordes l tablero0),g2)
    where fila= [0 | x <- [1..l]]
          tablero0 = [fila | x <- [1..l]]
          (posminas,g2)= listaMinas l nminas g desc
          
          

anadirMinas:: Int->[Int]->[[Int]]->[[Int]]
anadirMinas l [] tablero = tablero
anadirMinas l (x:xs) tablero = anadirMinas l xs (anadirMina l x tablero )
         
anadirMina:: Int->Int->[[Int]]->[[Int]]
anadirMina l pos tablero = parte1 ++ [nfila1] ++ [nfila2]++ [nfila3] ++ parte2
    where (parte1,resto) = splitAt (a-1) tablero
          (centro,parte2) = splitAt 3 resto
          (fila1,filas) = splitAt 1 centro
          (fila2,fila3)= splitAt 1 filas
          nfila1= sumarUnos b (concat fila1)
          nfila2= sumarUnosMina b (concat fila2)
          nfila3= sumarUnos b (concat fila3)
          (a,b)= casillas!!pos
          casillas=[(x,y) | x <- [1..l], y <- [1..l]]
          
sumarUnos::Int-> [Int]->[Int]
sumarUnos b fila = iz ++ fil ++ dr
    where (iz,resto) = splitAt (b-1) fila
          (centro, dr) = splitAt 3 resto
          fil= map sumaUno ( centro)

sumarUnosMina::Int-> [Int]->[Int]
sumarUnosMina b fila = iz ++ fil ++ dr
    where (iz,resto) = splitAt (b-1) fila
          (centro, dr) = splitAt 3 resto
          izz=take 1 centro
          drr= take 1 (reverse centro)
          fil= (map sumaUno izz) ++ [12] ++ (map sumaUno drr)
    

sumaUno::Int->Int
sumaUno a   
    | a==11 = a
    | a==12 = a
    | otherwise = a+1
  
          
listaMinas::(RandomGen g) => Int -> Int -> g -> [Int]-> ([Int], g)
listaMinas l 0 g lista = ([], g)
listaMinas l n g lista   
  | busca r lista = (fst(listaMinas l n g1 lista) , g1)
  | otherwise = (r:lista , g2)  
  where (r,g1)= randomR (0,t) g
        t= l*l-1
        (lista,g2)=listaMinas l (n-1) g1 (r:lista)
  
busca :: Ord a => a -> [a] -> Bool
busca x [] = False
busca x (y:ys)
  | x == y    = True
  | otherwise = busca x ys 
          
ponerBordes::Int -> [[Int]]->[[Int]]
ponerBordes l tab= fila:(map bordes tab)++[fila]
    where fila = [11 | x <- [1..(l+2)]] 
    
bordes::[Int]->[Int]
bordes fila = 11:fila++[11]
