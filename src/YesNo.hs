module YesNo
  ( YesNo(..)
  ) where

import Tree

class YesNo a where
  yesno :: a -> Bool

instance YesNo Bool where
  yesno = id

instance YesNo Char where
  yesno _ = True

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True
