import qualified Common.AdventAPI as AdventAPI

import Data.List.Split (splitOn, splitOneOf)

data Passport = Passport { byr :: Int       -- Birth Year
                         , iyr :: Int       -- Issue Year
                         , eyr :: Int       -- Expiration Year
                         , hgt :: String    -- Height
                         , hcl :: String    -- Hair Color
                         , ecl :: String    -- Eye Color
                         , pid :: String    -- Passport ID
                         , cid :: Int       -- Country ID
                         } deriving (Show)

defaultPassport :: Passport
defaultPassport = Passport (-1) (-1) (-1) "" "" "" "" (-1) 

-- | Add a new field to a passport. If it already exists, it is overwritten
addField :: Passport -> (String, String) -> Passport
addField p (field, value) = case field of
    "byr" -> p { byr = read value}
    "iyr" -> p { iyr = read value}
    "eyr" -> p { eyr = read value}
    "hgt" -> p { hgt = value}
    "hcl" -> p { hcl = value}
    "ecl" -> p { ecl = value}
    "pid" -> p { pid = value}
    "cid" -> p { cid = read value}
    _     -> p  -- just ignore unknown fields

-- | Check if all required fields are present
isValid :: Passport -> Bool
isValid p =
    byr p /= -1 &&
    iyr p /= -1 &&
    eyr p /= -1 &&
    not (null (hgt p)) &&
    not (null (ecl p)) &&
    not (null (hcl p)) &&
    not (null (pid p))

-- | Count occurences in the given list that satisfy 'cond'
count :: (a -> Bool) -> [a] -> Int
count cond = length . filter cond

parseInput :: String -> [Passport]
parseInput = map (parsePassport . splitOneOf " \n") . splitOn "\n\n"

parsePassport :: [String] -> Passport
parsePassport = foldr (foldFunc . splitOn ":") defaultPassport
    where foldFunc [field,value] p = addField p (field,value)
          foldFunc _ p = p

main :: IO()
main = do
    contents <- AdventAPI.readInput 4 "../session-cookie.txt" "../input"

    print . count isValid $ parseInput contents
