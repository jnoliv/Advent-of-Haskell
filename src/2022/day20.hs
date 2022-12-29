import Advent.Megaparsec (Parser, readParsedLines, sdecimal)
import Data.List (findIndex)
import Data.List.Index (deleteAt, insertAt)
import Data.Maybe (fromJust)

mix :: [(Int,Int)] -> [(Int,Int)]
mix l = foldl (mix1 len) l indexes
    where
        len     = length l
        indexes = [0 .. len - 1]

mix1 :: Int -> [(Int, Int)] -> Int -> [(Int, Int)]
mix1 len l n = insertAt i' e $ deleteAt i l
    where
        e@(_, num) = l !! i

        i  = fromJust $ findIndex ((== n) . fst) l
        i' = mod' i num (len - 1)

        mod' a b d
            | a + b == 0 = d
            | otherwise  = (a + b) `mod` d

findCoords :: [Int] -> Int
findCoords l = sum $ map (l !!) inds
    where
        len = length l
        i   = fromJust $ findIndex ((== 0)) l

        inds = map ((`mod` len) . (+) i) [1000, 2000, 3000]

-- |
-- >>> :main
-- 1591
-- 14579387544492
main :: IO ()
main = do
    input <- readParsedLines 2022 20 sdecimal

    let mixed   = mix (zip [0..] input)
        coords  = findCoords $ map snd mixed

        key       = 811589153
    
        decrypted = zip [0..] $ map (* key) input
        mixed10   = iterate mix decrypted !! 10
        coords10  = findCoords $ map snd mixed10
    
    print coords
    print coords10
