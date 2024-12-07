{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (readParsedLines, decimal, sepBy, some, Parser)
import Data.Bifunctor (second)

parser :: Parser (Int, [Int])
parser = (,) <$> decimal <* ":" <* some " "
             <*> decimal `sepBy` some " "

calibrate :: [Int -> Int -> Int] -> [Int] -> [Int]
calibrate fs (a:r) = calibrate' r a
    where
        calibrate' :: [Int] -> Int -> [Int]
        calibrate'    [] total = [total]
        calibrate' (n:r) total = concatMap (calibrate' r) . sequence (sequence fs total) $ n

(.||) :: (Read a1, Show a2, Show a3) => a2 -> a3 -> a1
a .|| b = read (show a ++ show b)

-- |
-- >>> :main
-- 6083020304036
-- 59002246504791
main :: IO ()
main = do
    input <- readParsedLines 2024 7 parser

    let ops1 = [(+), (*)]
        ops2 = [(+), (*), (.||)]

    print . sum . map fst . filter (uncurry elem) . map (second (calibrate ops1)) $ input
    print . sum . map fst . filter (uncurry elem) . map (second (calibrate ops2)) $ input
