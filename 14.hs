
insertionsort :: (a -> a -> Bool) -> [a] -> [a]
insertionsort f = foldl s [] where
    s [] y = y:[]
    s (x:xt) y = if f y x 
        then y:x:xt 
        else x:(s xt y)
