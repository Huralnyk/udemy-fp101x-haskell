{- 
double x = x + x
quadruple x = double (double x)
-}

-- Factorial of a positive integer:
factorial n = product [1..n]

-- Average of a list of integers:
average ns = sum ns `div` length ns

n = a `div` length xs
     where
     	a = 10
        xs = [1, 2, 3, 4, 5]

last1 ns = ns !! (length ns - 1)
last2 ns = head (reverse ns)
last3 ns = head (drop (length ns - 1) ns)

init1 ns = reverse (tail (reverse ns))
init2 ns = take (length ns - 1) ns

add' :: Int -> Int -> Int
add' x y = x + y

-- mult :: Int -> Int -> Int -> Int
-- mult x y z = x * y * z

bools = [True, False]
nums = [[1 :: Int,2,3], [1,2,3]]
add x y z = x+y+z
copy x = (x, x)
apply f x = f(x)

second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
double x = x*2
palindrome xs = reverse xs == xs
twice f x = f (f x)

-- Chapter 4: Defining Functions
even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1/n

abs1 :: Int -> Int
abs1 n = if n >= 0 then n else -n

abs2 :: Int -> Int
abs2 n | n >= 0    = n
	   | otherwise = -n

not1 :: Bool -> Bool
not1 False = True
not1 True = False

head1 :: [a] -> a
head1 (x:_) = x

tail1 :: [a] -> [a]
tail1 (_:xs) = xs

add1 = \x -> (\y -> x + y)

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0..n-1]

-- Chapter 4: Exercises
halve :: [a] -> ([a], [a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)
-- halve xs = (take n xs, drop n xs)
-- 		   where n = length xs `div` 2

third :: [a] -> a
-- third xs = head (tail (tail xs))
-- third xs = xs !! 2
third (_:_:x:_) = x

safetail :: [a] -> [a]
-- safetail xs = if null xs then [] else tail xs
-- safetail xs | null xs   = []
--             | otherwise = tail xs
-- safetail [] = []
-- safetail (_:xs) = xs
safetail 
  = \xs -> 
    case xs of
    	[] -> []
    	(_ : xs) -> xs


(||) :: Bool -> Bool -> Bool
-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False

-- False || False = False
-- _ || _ = True

False || a = a
True || _ = True

-- (&&) :: Bool -> Bool -> Bool
-- (&&) a b = if a then if b then True else False else False
-- (&&) a b = if a then b else False

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble :: Int -> Int
luhnDouble x = if x * 2 > 9 then x * 2 - 9 else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if (a + b + c + d) `mod` 10 == 0 then True else False


-- 3. Defining Functions. Lab

test :: Eq t => [[[t]]] -> Bool
test xs = True

-- 4. List Comprehensions. Lecture

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
















