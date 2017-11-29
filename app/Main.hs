module Main where

import qualified Geometry.Cube as Cube
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Sphere as Sphere
import Lib

import Data.Char
import Data.Function (on)
import Data.List
import qualified Data.Map as Map

main :: IO ()
main = someFunc

numUniques
  :: Eq a
  => [a] -> Int
numUniques = length . nub

-- Representing a polynomial 1 + 2x + 3x^2 as [1,2,3]
addPolynomials xs ys = map sum $ transpose [xs, ys]

{-# ANN any' "HLint: ignore Use any" #-}

any' pred xs = or $ map pred xs

{-# ANN all' "HLint: ignore Use all" #-}

all' pred xs = and $ map pred xs

-- take 5 $ iterate (*2) 1 == [1,2,4,8,16]
-- chain n f = foldr (.) id $ replicate n f
chain n f x = iterate f x !! n

splitAt' n xs
  | n <= 0 = ([], xs)
  | n >= length xs = (xs, [])
  | otherwise = (take n xs, chain n tail xs)
  -- span (/=' ') "This is a sentence" == break (==' ') "This is a sentence" && break (==' ') "This is a sentence" == ("This", " is a sentence")

-- sortUniq [] = []
-- sortUniq xs = foldr (\ x acc -> if null acc then [x] else (if x == head acc then acc else x : acc)) [] $ sort xs
sortUniq xs = map head $ group $ sort xs

isInfixOf' needle haystack =
  let nlen = length needle
  in foldr (\x acc -> ((take nlen x == needle) || acc)) False $ tails haystack

values =
  [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]

groupBySign
  :: (Num a, Ord a)
  => [a] -> [[a]]
-- groupBySign = groupBy (\ x y -> (x>0) == (y>0))
groupBySign = groupBy ((==) `on` (> 0))

sortByElemLength :: [[t]] -> [[t]]
sortByElemLength = sortBy (compare `on` length)

caesarEncode n = map (chr . (+ n) . ord)

caesarDecode n = map (chr . subtract n . ord)

phoneBook =
  [ ("betty", "555-2938")
  , ("bonnie", "452-2928")
  , ("patsy", "493-2928")
  , ("lucille", "205-2928")
  , ("wendy", "939-8282")
  , ("penny", "853-2492")
  ]

-- getValueByKey key = snd . head . filter (\ (k, v) -> k == key)
-- getValueByKey key [] = Nothing
-- getValueByKey key ((k, v):xs) = if k == key then Just v else getValueByKey key xs
getValueByKey key =
  foldr
    (\(k, v) acc ->
       if k == key
         then Just v
         else acc)
    Nothing

fromList
  :: Ord k
  => [(k, v)] -> Map.Map k v
fromList = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

dupPhoneBook =
  [ ("betty", "555-2938")
  , ("betty", "342-2492")
  , ("bonnie", "452-2928")
  , ("patsy", "493-2928")
  , ("patsy", "943-2929")
  , ("patsy", "827-9162")
  , ("lucille", "205-2928")
  , ("wendy", "939-8282")
  , ("penny", "853-2492")
  , ("penny", "555-2111")
  ]

phoneBookToMap
  :: Ord k
  => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (\v1 v2 -> v1 ++ ", " ++ v2)
