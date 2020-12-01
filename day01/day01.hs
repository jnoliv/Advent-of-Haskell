import Data.List (sort)

findPair :: Integer -> Int -> Int -> [Integer] -> Maybe (Integer, Integer)
findPair _ _ _ [] = Nothing
findPair sum l r list
        | l >= r = Nothing
        | curSum > sum  = findPair sum l (r - 1) list
        | curSum < sum  = findPair sum (l + 1) r list
        | curSum == sum = Just (left, right)
    where
        left  = list !! l
        right = list !! r
        curSum = left + right 

main :: IO()
main = do
    contents <- getContents
    let expenses = sort . map read . lines $ contents
    let size = length expenses

    print size

    case findPair 2020 0 (size - 1) expenses of
        Just (x,y) -> putStrLn $ "Result: " ++ show (x * y)
        Nothing   -> putStrLn "No entries sum to 2020"
