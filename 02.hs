
-- https://www.tutorialspoint.com/cplusplus-program-to-find-gcd-of-two-numbers-using-recursive-euclid-algorithm
gcd' n 0 = n
gcd' n m = gcd' m (n `mod` m)

lcm' n m = n * m `div` (gcd' n m) 
