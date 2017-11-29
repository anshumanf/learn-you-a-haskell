-- module Main where
-- import Lib
-- main :: IO ()
-- main = someFunc
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
sum'
  :: Num t
  => [t] -> t
-- sum' = foldl (+) 0
sum' = foldr (+) 0

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
