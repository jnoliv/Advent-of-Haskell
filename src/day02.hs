import qualified Common.AdventAPI as AdventAPI

import Data.List.Split (splitOneOf)

type PasswordData = ((Int, Int), Char, String)

-- | Boolean exclusive OR
xor :: Bool -> Bool -> Bool
xor = (/=)

-- | Count occurences in the given list that satisfy 'cond'
count :: (a -> Bool) -> [a] -> Int
count cond = length . filter cond

parseInput1 :: [String] -> PasswordData
parseInput1 input = ((l,r), c, pass)
    where
        l = read $ head input
        r = read $ input !! 1
        c = head $ input !! 2
        pass = input !! 4

parseInput2 :: [String] -> PasswordData
parseInput2 input = ((p1,p2), c, pass)
    where
        p1 = pred . read $ head input
        p2 = pred . read $ input !! 1
        c = head $ input !! 2
        pass = input !! 4

-- | Check if 'pass' has a count of characters 'c' between 'l' and 'r' 
isValid1 :: PasswordData -> Bool
isValid1 ((l,r), c, pass) = l <= cnt && cnt <= r
    where cnt = count (c==) pass

-- | Check if 'pass' has character 'c' at position 'p1' xor 'p2'
isValid2 :: PasswordData -> Bool
isValid2 ((p1,p2), c, pass) = (c1 == c) `xor` (c2 == c)
    where
        c1 = pass !! p1
        c2 = pass !! p2

main :: IO()
main = do
    contents <- AdventAPI.readInput 2 "../session-cookie.txt" "../input"
    let input1 = map (parseInput1 . splitOneOf " -:") . lines $ contents
    let input2 = map (parseInput2 . splitOneOf " -:") . lines $ contents

    putStrLn $ "Valid passwords with policy 1: " ++ (show . count isValid1 $ input1)
    putStrLn $ "Valid passwords with policy 2: " ++ (show . count isValid2 $ input2)
