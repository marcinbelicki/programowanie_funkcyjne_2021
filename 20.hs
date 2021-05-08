
--a
pozycja x l = pozycja_help x l 0 where
    pozycja_help _ [] _ = Nothing
    pozycja_help x (l:lt) i = 
        if x==l 
        then Just i 
        else pozycja_help x lt (i+1)

--b
drop' 0 l = Just l
drop' _ [] = Nothing
drop' n (l:lt) = drop' (n-1) lt

--c
sum' l = sum_help l 0 where
    sum_help [] s = s
    sum_help ((Just l):lt) s = sum_help lt (s+l)
    sum_help (Nothing:lt) s = sum_help lt s

