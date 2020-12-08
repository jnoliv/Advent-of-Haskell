import qualified Common.AdventAPI as AdventAPI

import qualified Data.Set as Set
import Data.List.Split (splitOn)

type AG = Set.Set Char  -- answer group

-- | Fold each group using 'f' with 'zero' as the base case and
-- return the sum of the sizes of each folded group
solve :: [[String]] -> (AG -> AG -> AG, AG) -> Int
solve answers (f, zero) = sum . map (Set.size . foldr foldFunc zero) $ answers
    where foldFunc x set = f set $ Set.fromList x

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 6

    let groups = map words . splitOn "\n\n" $ contents
    mapM_ (print . solve groups) [(Set.union, Set.empty), (Set.intersection, Set.fromList ['a'..'z'])]
