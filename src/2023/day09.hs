{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (readParsedLines, sdecimal, sepBy, some)

-- |
-- >>> :main
-- 1806615041
-- 1211
main :: IO ()
main = do
    input <- readParsedLines 2023 9 (sdecimal `sepBy` some " ")

    let diff l = zipWith (-) (tail l) l
        diffs  = takeWhile (not . all (== 0)) . iterate diff
        next   = sum . map last . diffs
        prev   = foldr ((-) . head) 0 . diffs

    print . sum $ map next input
    print . sum $ map prev input
