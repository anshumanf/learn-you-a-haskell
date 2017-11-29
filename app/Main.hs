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

qs
  :: Ord a
  => [a] -> [a]
qs [] = []
qs (x:xs) = qs [y | y <- xs, y <= x] ++ [x] ++ qs [y | y <- xs, y > x]

applyTwice f x = f (f x)

zw :: (a -> b -> c) -> [a] -> [b] -> [c]
zw f [] _ = []
zw f _ [] = []
zw f (x:xs) (y:ys) = f x y : zw f xs ys

fold f acc [] = acc
fold f acc (x:xs) = fold f (f acc x) xs

takeW pred [] = []
takeW pred (x:xs) =
  if pred x
    then x : takeW pred xs
    else []

cseq 1 = [1]
cseq n
  | odd n = n : cseq (3 * n + 1)
  | even n = n : cseq (n `div` 2)

-- main = print (length (filter (>15) (map (length . cseq) [1 .. 100])))
-- main = print (length (filter isLong (map cseq [1 .. 100])))
--   where isLong l = length l > 15
-- main = print (length (filter (\l -> length l > 15) (map cseq [1 .. 100])))
{-# ANN sum' "HLint: ignore Use sum" #-}

sum'
  :: Num t
  => [t] -> t
-- sum' = foldl (+) 0
sum' = foldr (+) 0

{-# ANN fr "HLint: ignore Use foldr" #-}

fr f acc [] = acc
fr f acc (x:xs) = x `f` fr f acc xs

-- fr f acc (x:xs) = f (x) (fr f acc xs)
fl f acc [] = acc
fl f acc (x:xs) =
  let acc' = acc `f` x
  in fl f acc' xs

fl' f acc [] = acc
fl' f acc (x:xs) =
  let acc' = acc `f` x
  in seq acc' $ fl' f acc' xs

-- mp f [] = []
-- mp f (x:xs) = f x : mp f xs
mp f = foldr (\x acc -> f x : acc) []

-- mx [] = error "empty list"
-- mx (x:xs) = if x > mx xs then x else mx xs
mx
  :: (Ord t)
  => [t] -> t
mx =
  foldr1
    (\x acc ->
       if x > acc
         then x
         else acc)

-- hd [] = error "empty list"
-- hd (x:xs) = x
hd :: [t] -> t
hd = foldr1 const

-- tl [] = error "empty list"
-- tl (x:xs) = xs
tl :: [t] -> ([t], [t])
tl = foldr (\x (_, xs) -> (xs, x : xs)) (error "empty list", [])

-- rev [] = []
-- rev (x:xs) = rev xs ++ [x]
rev :: [t] -> [t]
-- rev = foldl (\ acc x -> x : acc) []
rev = foldl (flip (:)) []

-- f ( g ( h x )) == f $ g $ h x
-- f( g ( h x )) == (f . g . h) x
-- sum (filter (> 10) (map (*2) [2..10])) == sum $ filter (> 10) $ map (*2) [2..10]
-- sum (filter (> 10) (map (*2) [2..10])) == (sum . filter (> 10) . map (*2)) [2..10]
sumOddSquares = map (^ 2) . filter odd

smallerThan10k = takeW (< 10 ^ 4)

oddSquareSum = sum . smallerThan10k . sumOddSquares

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

foo = map (+ 1) $ map (subtract 1) $ [1, 2]
