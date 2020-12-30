module Advent.Math (
    egcd, chineseRemainder
) where

-- | Extended Euclidean Algorithm
-- Returns (s, t, g) such that a*s + b*t = g = gcd a b
egcd :: Integral a => a -> a -> (a,a,a)
egcd a 0 = (1, 0, a)
egcd a b = (t, s - q*t, g)
    where (q, r)    = a `divMod` b
          (s, t, g) = egcd b r

-- | Chinese Remainder Theorem
-- Expects the input as a list of (r, n), where 'r' is the
-- remainder and 'n' the modulus, pairwise coprime. If these
-- constraints are met, returns the number that satisfies
-- the list of congruences wrapped in Just, otherwise Nothing.
chineseRemainder :: Integral a => [(a, a)] -> Maybe a
chineseRemainder [] = Nothing
chineseRemainder l =
    if and [gcd a b == 1 | (_,a) <- l, (_,b) <- l, a /= b]
        then Just . chineseRemainder' $ map (\(r, n) -> (r `mod` n, n)) l
        else Nothing

chineseRemainder' :: Integral a => [(a, a)] -> a
chineseRemainder' [(r,_)] = r
chineseRemainder' ((r1, n1):(r2, n2):cgs) = chineseRemainder' $ (r, n):cgs
    where (m1, m2, _) = egcd n1 n2
          n           = n1*n2
          r           = (r1 * m2 * n2 + r2 * m1 * n1) `mod` n
