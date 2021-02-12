import AdventAPI (readInputDefaults)
import Advent.Utils (md5)
import Data.List (isPrefixOf)

mine :: Int -> String -> Int
mine lead0 key = until ((prefix `isPrefixOf`) . md5 . (key ++) . show) succ 0
    where
        prefix = replicate lead0 '0'

-- |
-- >>> :main
-- 346386
-- 9958218
main :: IO ()
main = do
    key <- init <$> readInputDefaults 2015 4

    print $ mine 5 key
    print $ mine 6 key
