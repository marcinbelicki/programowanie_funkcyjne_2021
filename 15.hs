
iter 0 _ x = x
iter n f x = iter (n-1) f (f x)
