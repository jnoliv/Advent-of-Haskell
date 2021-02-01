module Advent.Math (
    egcd, chineseRemainder,
    powMod
) where

-- | Extended Euclidean Algorithm
-- Returns (s, t, g) such that a*s + b*t = g = gcd a b
egcd :: Integral a => a -> a -> (a, a, a)
egcd a 0 = (1, 0, a)
egcd a b = (t, s - q*t, g)
    where
        (q, r)    = a `divMod` b
        (s, t, g) = egcd b r

-- | Chinese Remainder Theorem
-- Expects the input as a list of (r, n), where 'r' is the
-- remainder and 'n' the modulus, pairwise coprime. If these
-- constraints are met, returns the number that satisfies
-- the list of congruences wrapped in Just, otherwise Nothing.
chineseRemainder :: Integral a => [(a, a)] -> Maybe a
chineseRemainder [] = Nothing
chineseRemainder l
    | valid     = Just $ chineseRemainder' normalized
    | otherwise = Nothing
    where
        valid      = and [gcd a b == 1 | (_,a) <- l, (_,b) <- l, a /= b]
        normalized = map (\(r, n) -> (r `mod` n, n)) l

chineseRemainder' :: Integral a => [(a, a)] -> a
chineseRemainder' [(r,_)]                     = r
chineseRemainder' ((r1, n1) : (r2, n2) : cgs) = chineseRemainder' ((r, n) : cgs)
    where
        (m1, m2, _) = egcd n1 n2
        n           = n1 * n2
        r           = (r1 * m2 * n2 + r2 * m1 * n1) `mod` n

-- | Modular exponentiation using repeated squaring
powMod :: Int -> Int -> Int -> Int
powMod base exponent modulus
    | modulus <= 0 = error "Non-positive modulus"
    | exponent < 0 = error "Negative exponent"
    | otherwise    = f base exponent 1
    where
        f b e acc
            | e == 0    = acc
            | odd e     = f b' e' (acc * b `mod` modulus)
            | otherwise = f b' e' acc
            where
                b' = b * b `mod` modulus
                e' = e `div` 2
