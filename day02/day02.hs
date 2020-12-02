import Data.List.Split (splitOneOf)

type PasswordData = ((Int, Int), Char, String)

-- | Boolean exclusive OR
xor :: Bool -> Bool -> Bool
True `xor` False  = True
False `xor` True  = True
_ `xor` _ = False

parseInput1 :: [String] -> PasswordData
parseInput1 input = ((l,r), c, pass)
    where
        l = read $ head input
        r = read $ input !! 1
        c = head $ input !! 2
        pass = input !! 4

isValid1 :: PasswordData -> Bool
isValid1 ((l,r), c, pass) = l <= count && count <= r
    where count = length . filter (c==) $ pass

parseInput2 :: [String] -> PasswordData
parseInput2 input = ((p1,p2), c, pass)
    where
        p1 = pred . read $ head input
        p2 = pred . read $ input !! 1
        c = head $ input !! 2
        pass = input !! 4

isValid2 :: PasswordData -> Bool
isValid2 ((p1,p2), c, pass) = (c1 == c) `xor` (c2 == c)
    where
        size = length pass
        c1 = if 0 <= p1 && p1 < size then pass !! p1 else ' '
        c2 = if 0 <= p2 && p2 < size then pass !! p2 else ' '

countValid :: (PasswordData -> Bool) -> [PasswordData] -> Int
countValid isValid = length . filter isValid

main :: IO()
main = do
    contents <- getContents
    let input1 = map (parseInput1 . splitOneOf " -:") . lines $ contents
    let input2 = map (parseInput2 . splitOneOf " -:") . lines $ contents

    putStrLn $ "Valid passwords with policy 1: " ++ (show . countValid isValid1 $ input1)
    putStrLn $ "Valid passwords with policy 2: " ++ (show . countValid isValid2 $ input2)
