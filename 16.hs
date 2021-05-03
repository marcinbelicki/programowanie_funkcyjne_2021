
dz test koniec dziel połącz p =
    if test p
        then koniec p
        else połącz (map (dz test koniec dziel połącz) (dziel p))

mergesort :: (Ord a) => [a] -> [a]
mergesort = dz t k d p where
    t (x:[]) = True
    t [] = True
    t _ = False
    k x = x
    d x = [take a x,drop a x] where
        a = div (length x) 2
    p [[],l] = l
    p [l,[]] = l
    p [x:xt,y:yt] = if x < y
        then x : (p [xt,y:yt])
        else y : (p [x:xt,yt])

karatsuba:: Int -> Int -> Int
karatsuba u v = karatsuba_help [u,v] where
    karatsuba_help = dz t k d p where
        t [_] = True
        t a = and (map (\x -> x <10) a)
        k [x] = x
        k [x,y] = x * y
        d [x,y] = [[x1,y1],[x2,y2],[x1+x2,y1+y2],[m]] where
            x1 = div x m
            x2 = mod x m
            y1 = div y m
            y2 = mod y m
            m = check (length (show (max x y))) where
                check x = if odd x then 10^(div (x+1) 2) else 10^(div x 2)
        p [x,y,z1,m] = x*m^2 + y + (z1-x-y) * m  
