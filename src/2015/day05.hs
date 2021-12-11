import Advent.API (readInputDefaults)
import Advent.Utils (count)
import qualified Data.Text as T

-- |
-- >>> isNice "ugknbfddgicrmopn"
-- True
-- 
-- >>> isNice "aaa"
-- True
-- 
-- >>> isNice "jchzalrnumimnmhp"
-- False
-- 
-- >>> isNice "haegwjzuvuyypxyu"
-- False
-- 
-- >>> isNice "dvszwmarrgswjxmb"
-- False
-- 
isNice :: String -> Bool
isNice str =
    count (`elem` "aeiou") str >= 3 &&
    hasDuplicate str &&
    all (not . (`isInfixOf` str)) ["ab", "cd", "pq", "xy"]

hasDuplicate :: String -> Bool
hasDuplicate (a : b : tail) = if a == b then True else hasDuplicate (b : tail)
hasDuplicate _ = False

isInfixOf :: String -> String -> Bool
isInfixOf "" _      = True
isInfixOf _ ""      = False
isInfixOf ifix str = (T.pack ifix) `T.isInfixOf` (T.pack str)

-- |
-- >>> isNewNice "qjhvhtzxzqqjkmpb"
-- True
-- 
-- >>> isNewNice "xxyxx"
-- True
-- 
-- >>> isNewNice "uurcxstgmygtbstg"
-- False
-- 
-- >>> isNewNice "ieodomkazucvgmuy"
-- False
-- 
isNewNice :: String -> Bool
isNewNice str = hasPair str && hasRepeat str

hasPair :: String -> Bool
hasPair (a : b : tail) = if [a,b] `isInfixOf` tail then True else hasPair (b : tail)
hasPair _                      = False

hasRepeat :: String -> Bool
hasRepeat (a : b : c : tail) = if a == c then True else hasRepeat (b : c : tail)
hasRepeat _                  = False

-- |
-- >>> :main
-- 238
-- 69
main :: IO ()
main = do
    strings <- lines <$> readInputDefaults 2015 5

    print . count isNice    $ strings
    print . count isNewNice $ strings
