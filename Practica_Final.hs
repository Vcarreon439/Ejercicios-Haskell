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
  show Empty = "Empty"
  show (Node x left right) = "Node " ++ show x ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

main :: IO ()
main = do
  let nums = [4, 2, 6, 1, 3, 5, 7]
      tree = buildTree nums
  print tree
