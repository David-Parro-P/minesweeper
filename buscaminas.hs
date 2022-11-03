-- Imports needed

-- "C:/Users/David/Desktop/Haskell/Practica/buscaminas.hs"
import System.IO
import Text.Read (readMaybe)
import Data.Maybe
import Data.Char (digitToInt)

readListOfInts :: String -> [Int]
readListOfInts = read

-- los 11 son bordes, los 12 son minas
-- dificultad facil tablero 9x9 con 10 minas

main :: IO ()
main = do
   let ruta = "C:/Users/David/Desktop/Haskell/Practica/tests.txt"
   contenido <- readFile ruta
   let tableroBackEnd = map (map (read::String->Int)) (map words (lines contenido))
   putStrLn contenido

type Tablero = [Int]

data Casilla = Mina | NoMina
   deriving (Eq,Show,Read)
data Estado = Flag  | Desc | NoDesc
   deriving (Eq,Show,Read)

-- Anadir un si la coordenada no es correcta o len incorrecta volver a introducir
descubrir :: [Int] -> Tablero -- Cambiar bool por un tablero 
descubrir _ = [2]
