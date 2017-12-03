module TrafficLight
  ( TrafficLight(..)
  ) where

data TrafficLight
  = Red
  | Yellow
  | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

instance Enum TrafficLight where
  toEnum x
    | x `mod` 3 == 0 = Red
    | x `mod` 3 == 1 = Yellow
    | x `mod` 3 == 2 = Green
  fromEnum Red = 0
  fromEnum Yellow = 1
  fromEnum Green = 2

instance Bounded TrafficLight where
  minBound = Red
  maxBound = Green
