import Advent.API (readInputDefaults)

fromSNAFU :: String -> Int
fromSNAFU = sum . zipWith (*) (iterate (*5) 1) . map toDigit . reverse
    where
        toDigit '=' = -2
        toDigit '-' = -1
        toDigit '0' =  0
        toDigit '1' =  1
        toDigit '2' =  2

toSNAFU :: Int -> String
toSNAFU 0 = "0"
toSNAFU n = snd . head . dropWhile ((/= 0) . fst) $ iterate f (n, "")
    where
        toChar i = "=-012" !! i

        f (m, l) = (m', (toChar r) : l)
            where
                (m', r) = (m + 2) `divMod` 5

-- |
-- >>> :main
-- 2-10==12-122-=1-1-22
main :: IO ()
main = do
    input <- lines <$> readInputDefaults 2022 25

    let total = sum $ map fromSNAFU input

    putStrLn $ toSNAFU total
