module MyList
  ( MyList(..)
  , (.++)
  ) where

infixr 5 :-:

data MyList a
  = Empty
  | (:-:) a
        (MyList a)
  deriving (Show, Read, Eq, Ord)

infixr 5 .++

(.++) :: MyList a -> MyList a -> MyList a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
