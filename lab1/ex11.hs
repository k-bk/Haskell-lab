roots :: (RealFloat a) => (a, a, a) -> (a, a)
roots (a, b, c) = 
        let 
            d = b ^ 2 - 4 * a * c 
            e = 2 * a 
        in ((-b + sqrt d) / e, (-b + sqrt d) / e)
