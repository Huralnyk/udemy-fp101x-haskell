all1 p xs = and (map p xs)
all2 p = and . map p
all3 p = not . any (not . p)
all4 p xs = foldl (&&) True (map p xs)
all5 p = foldr (&&) True . map p

