import Prelude hiding((^), and, concat, replicate, (!!), elem, sum, take, last)

-- (*) :: Int -> Int -> Int
-- m * 0 = 0
-- m * n = m + (m * (n-1))

-- product :: Num a => [a] -> a
-- product []     = 1
-- product (n:ns) = n * product ns

-- length :: [a] -> Int
-- length [] 	  = 0
-- length (_:xs) = 1 + length xs

-- reverse :: [a] -> [a]
-- reverse [] 	   = []
-- reverse (x:xs) = reverse xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] 				= [x]
insert x (y:ys) | x <= y 	= x : y : ys
				| otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []  	 = []
isort (x:xs) = insert x (isort xs)

-- drop :: Int -> [a] -> [a]
-- drop 0 xs	  = xs
-- drop _ [] 	  = []
-- drop n (_:xs) = drop (n-1) xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

qsort :: Ord a => [a] -> [a]
qsort []	 = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
	where 
		smaller = [a | a <- xs, a <= x]
		larger  = [b | b <- xs, b > x]

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs


-- Exercises

fac :: Int -> Int
-- fac n = product [1..n]
fac 0 = 1
fac n  | n < 0 = 0
	   | otherwise = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m ^ (n - 1))

euclid :: Int -> Int -> Int
euclid x y | x == y = x
		   | x > y = euclid (x - y) y
		   | otherwise = euclid x (y - x)

and :: [Bool] -> Bool
and [] 	   = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

(!!) :: [a] -> Int -> a
(x:_) !! 0  = x
(_:xs) !! n = xs !! (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) | x == y = True
			  | otherwise = elem x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y  	= x : merge xs (y:ys)
					| x == y 	= x : y : merge xs ys
					| otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
	where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
	where (ys, zs) = halve xs

sum :: Num a => [a] -> a
sum [] 	   = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take 0 xs  	  = xs
take n [] 	  = []
take n (x:xs) = x : take (n - 1) xs

last :: [a] -> a
last [x] 	= x
last (_:xs) = last xs
