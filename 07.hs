
-- rekursywnie
fib_r 0 = 0
fib_r 1 = 1
fib_r n = fib_r (n-1) + fib_r (n - 2)

-- iteracyjnie
fib n = fib_help n 1 0 where
    fib_help 0 _ acc2 = acc2
    fib_help 1 acc1 _ = acc1
    fib_help n acc1 acc2 = fib_help (n-1) (acc1 + acc2) acc1
