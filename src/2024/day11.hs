{-# LANGUAGE LambdaCase, OverloadedStrings #-}

import Advent.Megaparsec (decimal, Parser, readParsed, sepBy)
import Data.Bifunctor (bimap)
import Data.MemoTrie

parser :: Parser [Int]
parser = decimal `sepBy` " "

nDigits :: Int -> Int
nDigits = length . show

blink :: Int -> Int -> Int
blink = memo2 $ \cases
    _ 0                   -> 1
    0 b                   -> blink 1 (b - 1)
    n b | odd (nDigits n) -> blink (n * 2024) (b - 1)
    n b                   -> blink n1 (b - 1) + blink n2 (b - 1)
        where
            (n1, n2) = bimap read read . splitAt (nDigits n `div` 2) $ show n

-- |
-- >>> :main
-- 184927
-- 220357186726677
main :: IO ()
main = do
    input <- readParsed 2024 11 parser

    print . sum . map (`blink` 25) $ input
    print . sum . map (`blink` 75) $ input
