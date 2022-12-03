import Advent.Megaparsec
import Data.Char (isLower, isUpper, ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)

splitInHalf :: String -> (String, String)
splitInHalf s = (take l s, drop l s)
    where l = length s `div` 2

-- | Returns an item type's priority, where
-- a through z have priorities 1 through 26
-- and A through Z have priorities 27 through 52
--
-- >>> map priority "azAZ"
-- [1,26,27,52]
priority :: Char -> Int
priority c
    | isLower c = ord c - 96
    | isUpper c = ord c - 38 -- (-65 + 27)

-- | Intersects 3 lists given as a 3 element list of lists.
--
-- >>> intersect3 ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"]
-- "rr"
intersect3 :: Eq a => [[a]] -> [a]
intersect3 [a, b, c] = intersect a (intersect b c)

-- |
-- >>> :main
-- 7848
-- 2616
main :: IO ()
main = do
    input <- readParsedLines 2022 3 (many letterChar)

    let itemsPerCompartment = map splitInHalf input
        priorities          = map (priority . head . uncurry intersect) itemsPerCompartment

        groups              = chunksOf 3 input
        groupPriorities     = map (priority . head . intersect3) groups

    print $ sum priorities
    print $ sum groupPriorities
