

kwad:: (Floating a) => a -> a -> a -> [a]
kwad a b c = let d = (b ^ 2) - 4.0 * a * c in 
    if d < 0 then
        []  
    else
        [(-b - sqrt d)/(2*a)]