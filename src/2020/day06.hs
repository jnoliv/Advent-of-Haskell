import Advent.API
import qualified Data.Set as Set
import Data.List.Split (splitOn)

type AG = Set.Set Char  -- answer group

-- | Fold each group using 'f' with 'zero' as the base case and
-- return the sum of the sizes of each folded group
solve :: [[String]] -> (AG -> AG -> AG) -> AG -> Int
solve answers f zero = sum . map (Set.size . foldr foldFunc zero) $ answers
    where foldFunc x set = f set $ Set.fromList x

-- |
-- >>> :main
-- 7027
-- 3579
main :: IO()
main = do
    contents <- readInputDefaults 2020 6

    let groups = map words . splitOn "\n\n" $ contents
    mapM_ print $ zipWith (solve groups) [Set.union, Set.intersection] [Set.empty, Set.fromList ['a'..'z']]
