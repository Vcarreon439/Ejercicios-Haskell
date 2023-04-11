import Data.List (foldl')

--Victor Hugo Carreon Pulido - 192310436
--Edgar Eduardo Arguijo Vazquez - 192310252

--https://www.glc.us.es/~jalonso/vestigium/i1m2014-ejercicios-sobre-arboles-binarios-en-haskell/

--Para definir el arbol
data Tree a = Empty | Node a (Tree a) (Tree a)

--Para insertar un elemento en el arbol
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

--Para construir el arbol
buildTree :: Ord a => [a] -> Tree a
buildTree = foldl' (flip insert) Empty

--Para imprimir el arbol
instance Show a => Show (Tree a) where
  show Empty = "Vacio"
  show (Node x left right) = "Nodo " ++ show x ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

--Flujo principal del programa
main :: IO ()
main = do
  let nums = []
  input nums

--Para leer los numeros en funcion de n
input::[String]->IO()
input xs = do
  print("Introduce un numero o (n/N) para salir")
  x <- getLine
  if x == "n" || x == "N"
    then do
      (print (buildTree(convertir(xs))))
      print((inorden (buildTree(convertir(xs)))))
      --Generame la funcion inorden
      print("Deseas volver a ejecutar el programa? (s/n)")
      y <- getLine
      if y == "s" || y == "S"
        then do
          main
        else do
          print("Gracias por usar el programa")
      return()
    else (input (xs++[x]))

--Para convertir de String a Int
convertir :: [String] -> [Int]
convertir xr = map read xr 

--Para ordenar el arreglo
inorden :: Tree a -> [a]
inorden Empty = []
inorden (Node x left right) = inorden left ++ [x] ++ inorden right