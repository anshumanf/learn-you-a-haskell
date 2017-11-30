module Shapes
  ( Point(..)
  , Shape(..)
  , surface
  , nudge
  , baseCircle
  , baseRect
  ) where

data Point =
  Point Float
        Float
  deriving (Show)

data Shape
  = Circle Point
           Float
  | Rectangle Point
              Point
  deriving (Show)

{-# ANN surface "HLint: ignore Move brackets to avoid $" #-}

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
  (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) p q = Circle (Point (x + p) (y + q)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) p q =
  Rectangle (Point (x1 + p) (y1 + q)) (Point (x2 + p) (y2 + q))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
