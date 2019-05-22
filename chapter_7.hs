import Prelude hiding (map, filter, sum, product, or, and, foldr, foldl, length, reverse, (.))

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map :: (a -> b) -> [a] -> [b]
-- map f xs = [f x | x <- xs]

map f [] 	 = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
-- filter p xs = [x | x <- xs, p x]

filter p [] 	= []
filter p (x:xs) | p x 		= x : filter p xs
				| otherwise = filter p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

sum :: Num a => [a] -> a
-- sum [] = 0
-- sum (x:xs) = x + sum xs
sum = foldr (+) 0

product :: Num a => [a] -> a
-- product [] = 1
-- product (x:xs) = x * product xs
product = foldr (*) 1

or :: [Bool] -> Bool
-- or [] = False
-- or (x:xs) = x || or xs
or = foldr (||) False

and :: [Bool] -> Bool
-- and [] = True
-- and (x:xs) = x && and xs
and = foldr (&&) True

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] 	 = v
foldr f v (x:xs) = f x (foldr f v xs)

length :: [a] -> Int
-- length []	  = 0
-- length (_:xs) = 1 + length xs
-- length = foldr (\_ n -> 1 + n) 0
length = foldl (\n _ -> n + 1) 0

reverse :: [a] -> [a]
-- reverse [] 	   = []
-- reverse (x:xs) = reverse xs ++ [x]
-- reverse = foldr (\x xs -> xs ++ [x]) []
reverse = foldl (\xs x -> x:xs) []

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] 	 = v
foldl f v (x:xs) = foldl f (f v x) xs

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
