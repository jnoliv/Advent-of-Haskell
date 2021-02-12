import AdventAPI (readInputDefaults)
import Data.List (transpose, sort, sortOn, group)

correct :: ([String] -> String) -> [String] -> String
correct f cols = map xFrequent cols
    where
        xFrequent = head . f . sortOn length . group . sort

-- |
-- >>> :main
-- qqqluigu
-- lsoypmia
main :: IO ()
main = do
    columns <- transpose . lines <$> readInputDefaults 2016 6

    putStrLn $ correct last columns
    putStrLn $ correct head columns
