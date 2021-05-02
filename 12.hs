

foldr':: (a->b->b) -> b -> [a] -> b
foldr' _ a [] = a
foldr' f a (l:lt) = f l (foldr' f a lt)
