import Advent.Megaparsec(Parser, readParsedLines, many, letterChar)
import Advent.Utils (intersects)
import Data.Char (isLower, isUpper, ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)

splitInHalf :: String -> (String, String)
splitInHalf s = splitAt l s
    where l = length s `div` 2

-- | Returns an item type's priority, where
-- a through z have priorities 1 through 26
-- and A through Z have priorities 27 through 52
--
-- >>> map priority "azAZ"
-- [1,26,27,52]
priority :: Char -> Int
priority c
    | isLower c = ord c - ord 'a' + 1
    | isUpper c = ord c - ord 'A' + 27

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
        groupPriorities     = map (priority . head . intersects) groups

    print $ sum priorities
    print $ sum groupPriorities
