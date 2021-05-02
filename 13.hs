
--a
prod :: (Num a) => [a] -> a
prod = foldl (*) 1

--b
length' :: [a] -> Int
length' = foldl (\x _ -> x + 1) 0

--c
and' :: [Bool] -> Bool
and' = foldl (&&) True

--d
nwd :: [Int] -> Int
nwd = foldl gcd 0

--e
delete' :: (Eq a) => a -> [a] -> [a]
delete' x = foldr check [] where
    check a l | a == x = l
    check y l = y:l

--f
map' :: (a->b) -> [a] -> [b]
map' f = foldr (\a b -> (f a):b) []

--g
reverse' :: [a] -> [a]
reverse' = foldl (\x y -> y:x) []

--h
filter' :: (a -> Bool) -> [a] -> [a]
filter' pred = foldr (\a b -> if pred a then a:b else b) []

--i
forall :: (a->Bool) -> [a] -> [Bool]
forall pred = foldr (\a b -> (pred a):b) []
