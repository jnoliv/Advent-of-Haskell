import AdventAPI (readInputDefaults)

-- |
-- >>> rle "1"
-- 11
--
-- >>> rle "11"
-- 21
--
-- >>> rle "21"
-- 1211
--
-- >>> rle "1211"
-- 111221
--
-- >>> rle "111221"
--111221
rle :: String -> String
rle str = go (head str) 0 str
    where
        go c n []       = show n ++ [c]
        go c n (x : xs)
            | x == c    = go c (n + 1) xs
            | otherwise = show n ++ [c] ++ go x 1 xs

-- |
-- >>> :main
-- 252594
-- 3579328
main :: IO ()
main = do
    sequence <- init <$> readInputDefaults 2015 10

    let infRLE = iterate rle sequence

    print . length $ infRLE !! 40
    print . length $ infRLE !! 50
