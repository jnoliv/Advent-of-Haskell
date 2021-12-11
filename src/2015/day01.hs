import Advent.API (readInputDefaults)

toNum :: String -> [Int]
toNum = map (\c -> if c == '(' then 1 else -1)

stepsTilBasement :: [Int] -> Int
stepsTilBasement = snd . foldl f (0,0)
    where f r@(floor, steps) dir = if floor == -1 then r else (floor + dir, steps + 1)

-- |
-- >>> :main
-- 232
-- 1783
main :: IO ()
main = do
    steps <- toNum <$> readInputDefaults 2015 1

    print . sum              $ steps
    print . stepsTilBasement $ steps
