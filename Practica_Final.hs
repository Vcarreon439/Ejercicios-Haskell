import Data.List (foldl')

data Tree a = Empty | Node a (Tree a) (Tree a)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

buildTree :: Ord a => [a] -> Tree a
buildTree = foldl' (flip insert) Empty

instance Show a => Show (Tree a) where
  show Empty = "Vacio"
  show (Node x left right) = "Nodo " ++ show x ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

main :: IO ()
main = do
  let nums = []
  input nums


input::[String]->IO()
input xs = do
  print("Introduce un numero o (n/N) para salir")
  x <- getLine
  if x == "n" || x == "N"
    then do
      (print (buildTree(convertir(xs))))
      print("Deseas volver a ejecutar el programa? (s/n)")
      y <- getLine
      if y == "s" || y == "S"
        then do
          main
        else do
          print("Gracias por usar el programa")
      return()
    else (input (xs++[x]))

convertir :: [String] -> [Int]
convertir xr = map read xr 