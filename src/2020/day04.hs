import Advent.API
import Advent.Utils (count)
import Data.Char (isDigit)
import Data.List.Split (splitOn)

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
isFilled :: Passport -> Bool
isFilled p = all isPresent [byr, iyr, eyr] && all isPresent [hgt, hcl, ecl, pid]
    where isPresent f = f p /= f defaultPassport

-- | Check if all required fields are present and valid
isValid :: Passport -> Bool
isValid Passport{byr=byr, iyr=iyr, eyr=eyr, hgt=hgt, hcl=hcl, ecl=ecl, pid=pid} =
    (1920 <= byr && byr <= 2002) &&
    (2010 <= iyr && iyr <= 2020) &&
    (2020 <= eyr && eyr <= 2030) &&
    isValidHeight hgt &&
    isValidHairColor hcl &&
    isValidEyeColor ecl &&
    isValidPassportID pid

isValidHeight :: String -> Bool
isValidHeight str =
    let num  = read $ takeWhile isDigit str
        unit = dropWhile isDigit str
    in case unit of
        "cm" -> 150 <= num && num <= 193
        "in" -> 59 <= num && num <= 76
        _    -> False

isValidHairColor :: String -> Bool
isValidHairColor ('#':as) = length as == 6 && all isHex as
isValidHairColor _ = False

isValidEyeColor :: String -> Bool
isValidEyeColor = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPassportID :: String -> Bool
isValidPassportID str = length str == 9 && all isDigit str

-- | Check if character is hexadecimal digit
isHex :: Char -> Bool
isHex = flip elem (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])

-- | Parse the input into a list of Passport records
parseInput :: String -> [Passport]
parseInput = map (parsePassport . words) . splitOn "\n\n"

-- | Parse a list of fields into a Passport record
parsePassport :: [String] -> Passport
parsePassport = foldr (foldFunc . splitOn ":") defaultPassport
    where foldFunc [field,value] p = addField p (field,value)
          foldFunc _ p = p

-- |
-- >>> :main
-- 264
-- 224
main :: IO()
main = do
    contents <- readInputDefaults 2020 4

    let passports = parseInput contents

    print . count isFilled $ passports
    print . count isValid  $ passports
