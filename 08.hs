

-- rekursywnie
exp_r b 0 = 1
exp_r b e = check (e `mod` 2) where
    check 0 = (exp_r b (div e 2)) ^ 2
    check _ = b * (exp_r b (e-1))

-- iteracyjnie
exp_i b e = exp_i_help b e 1 where
    exp_i_help _ 0 acc = acc
    exp_i_help b e acc = check (e `mod` 2) where
        check 0 = exp_i_help (b ^ 2) (div e 2) acc
        check _ = exp_i_help b (e - 1) (b*acc)