import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, curry, uncurry)

mapFilter :: Eq a => (a -> b) -> (a -> Bool) -> [a] -> [b]
-- mapFilter f p xs = [f x | x <- xs, p x]
mapFilter f p xs = map f (filter p xs)

all :: (a -> Bool) -> [a] -> Bool
-- all p [] = True
-- all p (x:xs) = p x && all p xs
-- all p xs = foldr (&&) True (map p xs)
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
-- any p [] = False
-- any p (x:xs) = p x || any p xs
-- any p xs = foldr (||) False (map p xs)
any p = or . map p

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] 	   = []
takeWhile p (x:xs) | p x = x : takeWhile p xs
				   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] 	   = []
dropWhile p (x:xs) | p x = dropWhile p xs
				   | otherwise = (x:xs)

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x:xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x, y) -> f x y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x 	   = []
			   | otherwise = h x : unfold p h t (t x)