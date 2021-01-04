import AdventAPI (readInputDefaults)

-- |
-- >>> :main
-- 232
main :: IO ()
main = do
    directions <- map (\c -> if c == '(' then 1 else -1) <$> readInputDefaults 2015 1
    print . sum $ directions
