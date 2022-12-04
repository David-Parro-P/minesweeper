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


crearTablero::(RandomGen g) => Int -> g -> [Int]-> ([[Int]],g)
crearTablero nminas g desc  = (anadirMinas posminas (ponerBordes tablero0) ,g2)
    where fila= [0 | x <- [1..9]]
          tablero0 = [fila | x <- [1..9]]
          (posminas,g2)= listaMinas nminas g desc
          
          

anadirMinas:: [Int]->[[Int]]->[[Int]]
anadirMinas [] tablero = tablero
anadirMinas (x:xs) tablero = anadirMinas  xs (anadirMina x tablero )
         
anadirMina:: Int->[[Int]]->[[Int]]
anadirMina pos tablero = parte1 ++ [nfila1] ++ [nfila2]++ [nfila3] ++ parte2
    where (parte1,resto) = splitAt (a-1) tablero
          (centro,parte2) = splitAt 3 resto
          (fila1,filas) = splitAt 1 centro
          (fila2,fila3)= splitAt 1 filas
          nfila1= sumarUnos b (concat fila1)
          nfila2= sumarUnosMina b (concat fila2)
          nfila3= sumarUnos b (concat fila3)
          (a,b)= casillas!!pos
          casillas=[(x,y) | x <- [1..9], y <- [1..9]]
          
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
  
          
listaMinas::(RandomGen g) => Int -> g -> [Int]-> ([Int], g)
listaMinas 0 g lista = ([], g)
listaMinas n g lista   
  | busca r lista = (fst(listaMinas n g1 lista) , g1)
  | otherwise = (r:lista , g2)  
  where (r,g1)= randomR (0,80) g
        (lista,g2)=listaMinas (n-1) g1 (r:lista)
  
busca :: Ord a => a -> [a] -> Bool
busca x [] = False
busca x (y:ys)
  | x == y    = True
  | otherwise = busca x ys 
          
ponerBordes::[[Int]]->[[Int]]
ponerBordes tab= fila:(map bordes tab)++[fila]
    where fila = [11 | x <- [1..11]] 
    
bordes::[Int]->[Int]
bordes fila = 11:fila++[11]
