import AdventAPI (readInputDefaults)
import Advent.Utils (count)

combinations :: [Int] -> Int -> [[Int]]
combinations containers liters = go containers liters []
    where
        go []       0      selected = [selected]
        go []       _      selected = []
        go (c : cs) liters selected
            | liters <  0 = []
            | liters == 0 = [selected]
            | otherwise   = use ++ dont
            where use  = go cs (liters - c) (c : selected)
                  dont = go cs liters       selected 

-- |
-- >>> :main
-- 1638
-- 17
main :: IO ()
main = do
    containers <- (map read . lines) <$> readInputDefaults 2015 17

    let validCombinations = combinations containers 150
        optimalSize       = minimum $ map length validCombinations

    print $ length validCombinations
    print $ count ((== optimalSize) . length) validCombinations
