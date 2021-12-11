import Advent.API (readInputDefaults)
import Data.List (findIndex)
import Data.Maybe (fromJust)

divisors :: (Int -> Int -> [Int]) -> Int -> [Int]
divisors _ 1 = [1]
divisors _ 2 = [1,2]
divisors _ 3 = [1,2,3]
divisors f n = f 1 n ++ divs
    where
        limit = floor . sqrt . fromIntegral $ n
        divs  = concat [f i q | i <- [2 .. limit - 1], let (q, r) = quotRem n i, r == 0]

lowestWithAtLeast :: Int -> Int -> [[Int]] -> Int
lowestWithAtLeast scale lb = fromJust . findIndex (>= lb) . map ((*) scale . sum)

validDivisors :: Int -> Int -> [Int]
validDivisors a b
    | a > 50 && b > 50 = []
    | a > 50           = [a]
    |           b > 50 = [b]
    | otherwise        = [a,b]

-- |
-- >>> :main
-- 831600
-- 884520
main :: IO ()
main = do
    bound <- read <$> readInputDefaults 2015 20

    print . lowestWithAtLeast 10 bound $ map (divisors (\a b -> [a,b])) [0..]
    print . lowestWithAtLeast 11 bound $ map (divisors validDivisors  ) [0..]
