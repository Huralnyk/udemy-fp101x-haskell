import Data.Char
import Prelude hiding (map, iterate)

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
	where weights = iterate (*2) 1
-- bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin = unfold (==0) (`mod` 2) (`div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (parity . make8 . int2bin . ord)

parity :: [Bit] -> [Bit]
parity bits = if odd ones then bits ++ [1] else bits ++ [0]
	where ones = sum bits

check :: [Bit] -> [Bit]
check bits = if even ones then init bits else error "parity error"
	where ones = sum bits

chop :: Int -> [Bit] -> [[Bit]]
-- chop8 []   = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop n = unfold null (take n) (drop n)

decode :: [Bit] -> String
decode = map (chr . bin2int . check) . (chop 9)

transmit :: String -> String
transmit = decode . faltyChannel . encode

channel :: [Bit] -> [Bit]
channel = id

faltyChannel :: [Bit] -> [Bit]
faltyChannel = tail

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x 	   = []
			   | otherwise = h x : unfold p h t (t x)

map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) (drop 1)

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (\_ -> False) id f

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhn :: [Int] -> Bool
luhn n = f n `mod` 10 == 0 
	where f = sum . (map (\n -> if n > 9 then n - 9 else n)) . reverse . (altMap id (*2)) . reverse
