module Tree
  ( Tree(..)
  , singleton
  , treeInsert
  , treeElem
  ) where

data Tree a
  = EmptyTree
  | Node a
         (Tree a)
         (Tree a)
  deriving (Show, Read, Eq)

-- How does one fold a tree using infix traversal??
-- instance Foldable Tree where
--   toList EmptyTree = []
--   toList (Node a left right) = toList left ++ [a] ++ toList right
--   foldr g acc EmptyTree = acc
--   foldr g acc (Node x left right) = g x (foldr g acc right)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
  | x < y = Node y (treeInsert x left) right
  | x > y = Node y left (treeInsert x right)
  | x == y = Node x (treeInsert x left) right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
  | x < y = treeElem x left
  | x > y = treeElem x right
  | x == y = True
