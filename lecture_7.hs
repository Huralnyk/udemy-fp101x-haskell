import Control.Monad
import Char

newtype Parser a = P (String -> [(a,String)])

instance Monad Parser where
	return v 	= P (\inp -> [(v,inp)])
	p >>= f     = error "You must implement (>>=)"

instance MonadPlus Parser where
	mzero 		= P (\_ -> [])
	p `mplus` q = P (\inp -> case parse p inp of
							[] -> parse q inp
							[(v,out)] -> [(v,out)])

item :: Parser Char
item = P (\inp -> case inp of
					[] 	   -> []
					(x:xs) -> [(x,xs)])

failure :: Parser a
failure = P (\_ -> [])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- p :: Parser (Char,Char)
-- p = do x <- item
--        item
--        y <- item
--        return (x,y)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then
           	return x
           else
           	failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (x ==)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

p :: Parser String
p = do char '['
       d <- digit
       ds <- many (do char ',' 
       	              digit)
       char ']'
       return (d:ds)
