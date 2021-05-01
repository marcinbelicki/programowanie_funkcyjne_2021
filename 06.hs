kwad a b c = check (signum d) where
        d = (b ^ 2) - 4.0 * a * c
        p = - b / (2 * a)
        q = sqrt d / (2 * a)
        check 1 = [p - q,p + q]
        check 0 = [p]
        check _ = []