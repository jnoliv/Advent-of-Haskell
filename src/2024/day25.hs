import Advent.API (readInputDefaults)
import Advent.Utils (count)
import Data.List (partition, transpose)
import Data.List.Split (splitOn)

isLock :: [[Char]] -> Bool
isLock = all (== '#') . head

toHeights :: [[Char]] -> [Int]
toHeights = map (count (== '#')) . transpose

-- |
-- >>> :main
-- 
main :: IO ()
main = do
    (locks,keys) <- partition isLock . map lines . splitOn "\n\n" <$> readInputDefaults 2024 25

    let maxHeight = 7

        locksH = map toHeights locks
        keysH  = map toHeights keys

    print $ length [() | lock <- locksH, key <- keysH, all (<= maxHeight) $ zipWith (+) lock key]
