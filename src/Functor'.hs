module Functor'
  ( Functor'(..)
  ) where

import Tree

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
  fmap' g Nothing = Nothing
  fmap' g (Just x) = Just (g x)

{-# ANN module "HLint: ignore Use map" #-}

instance Functor' [] where
  fmap' g [] = []
  fmap' g (x:xs) = g x : fmap' g xs

instance Functor' Tree where
  fmap' g EmptyTree = EmptyTree
  fmap' g (Node x left right) = Node (g x) (fmap' g left) (fmap' g right)
