import Advent.API (readInputDefaults)
import Data.List (permutations, transpose)
import Data.List.Split (chunksOf)

valid :: [Int] -> Bool
valid [a,b,c] = a + b > c && a + c > b && b + c > a

-- | Select triangles in groups of 3 elements from each column
-- >>> splitVertically [[1,2,3], [4,5,6], [7,8,9], [10,11,12], [13,14,15], [16,17,18]]
-- [[1,4,7],[10,13,16],[2,5,8],[11,14,17],[3,6,9],[12,15,18]]
splitVertically :: [[Int]] -> [[Int]]
splitVertically l = concatMap (chunksOf 3) $ transpose l

-- |
-- >>> :main
-- 1032
-- 1838
main :: IO ()
main = do
    triangles <- (map (map read . words) . lines) <$> readInputDefaults 2016 3

    print . length $ filter valid triangles
    print . length . filter valid $ splitVertically triangles
