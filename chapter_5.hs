
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x,y) <- grid n n, x /= y]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], 
					 y <- [1..n],
					 z <- [1..n],
					 x ^ 2 + y ^ 2 == z ^ 2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [i | i <- [0..n]])
	where n = length xs - 1

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]