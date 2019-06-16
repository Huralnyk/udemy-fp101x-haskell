data Expr = Val Int | Add Expr Expr | Mult Expr Expr

value :: Expr -> Int
value (Val n)    = n
value (Add x y)  = value x + value y
value (Mult x y) = value x * value y 

-- type Cont = [Op]

-- data Op = EVAL Expr | ADD Int

-- eval :: Expr -> Cont -> Int
-- eval (Val n) c   = exec c n
-- eval (Add x y) c = eval x (EVAL y : c)

-- exec :: Cont -> Int -> Int
-- exec [] 		  n = n
-- exec (EVAL y : c) n = eval y (ADD n : c)
-- exec (ADD n : c)  m = exec c (n + m)

-- value :: Expr -> Int
-- value e = eval e []

-- Exercises

-- folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
-- folde f g (Val n) = f n
-- folde f g (Add x y) = g (folde f g x) (folde f g y)

-- eval :: Expr -> Int
-- eval = folde id (+)

-- size :: Expr -> Int
-- size = folde (\_ -> 1) (+)

expr :: Expr
expr = (Mult (Val 2) (Add (Add (Val 2) (Val 3)) (Val 4)))
