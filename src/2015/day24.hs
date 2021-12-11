import Advent.API (readInputDefaults)
import Advent.Utils (combinations)
import Data.List (sort)

-- | The key detail here is that we don't really need to consider how we
-- partition all groups, we just need to find all the possible package
-- arrangements that sum to total_weight / num_parts.
-- Given that we also prefer smaller arrangements, we start with smaller
-- size combinations and only if none match the required weight, do we
-- search bigger combinations.
-- I do, however, think it would be possible to craft an input that would
-- make this fail, if it is possible to find a combination that sums to
-- the required weight but the remaining numbers cannot be split into
-- equal total weight parts. Does not seem to be the case with the given
-- input, thankfully.
solve :: [Int] -> Int -> Int
solve []      _      = 0
solve _       0      = 0
solve weights nComps = go 1
    where
        wpc      = sum weights `div` nComps  -- weight per compartment

        go len
            | null parts = go (len + 1)
            | otherwise  = minimum $ map product parts
            where 
                parts = filter ((== wpc) . sum) $ combinations len weights

-- |
-- >>> :main
-- 11846773891
-- 80393059
main :: IO ()
main = do
    input <- map read . lines <$> readInputDefaults 2015 24

    print $ solve input 3
    print $ solve input 4
