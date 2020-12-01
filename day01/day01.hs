findMatch :: Integer -> [Integer] -> Maybe Integer
findMatch _ [] = Nothing
findMatch x (y:ys) =
    if x + y == 2020
        then Just y
        else findMatch x ys

findEntries :: [Integer] -> Maybe (Integer, Integer)
findEntries [] = Nothing
findEntries (x:xs) =
    case findMatch x xs of
        Just y  -> Just (x, y)
        Nothing -> findEntries xs

main :: IO()
main = do
    contents <- getContents
    let expenses = map read . lines $ contents

    case findEntries expenses of
        Just (x,y) -> putStrLn $ "Result: " ++ show (x * y)
        Nothing   -> putStrLn "No entries sum to 2020"
