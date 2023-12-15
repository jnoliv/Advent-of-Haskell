{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (decimal, sepBy, some, punctuationChar, Parser, readParsedLines)
import Data.Array((!), listArray)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.List (intercalate)

parser :: Parser (String, [Int])
parser = (,) <$> some punctuationChar <* " " <*> decimal `sepBy` ","

countArrangements :: String -> [Int] -> Int
countArrangements springs nums = memo ! (0, 0)
    where
        maxSs = length springs
        maxNs = length nums

        memo = listArray ((0,0), (maxSs, maxNs)) [arrangements i j | i <- [0..maxSs], j <- [0..maxNs]]

        arrangements indS indN
            | indS == maxSs && indN == maxNs = 1
            |                  indN == maxNs = bool 0 1 (notElem '#' $ drop indS springs)
            | (maxSs - indS) < lenNs         = 0
            | at indS == '.'                 = memo ! (indS + 1, indN)
            | at indS == '?'                 = memo ! (indS + 1, indN) + damaged indS indN
            | at indS == '#'                 =                           damaged indS indN
            | otherwise                      = error $ "invalid character " ++ show (at indS)
            where
                nums' = drop indN nums
                lenNs = sum nums' + length nums' - 1

        damaged indS indN
            | '.' `elem` springs'   = 0
            | (indS' + 1) == maxSs  = 1
            | at (indS' + 1) == '#' = 0
            | otherwise             = memo ! (indS' + 2, indN + 1)
            where
                num   = nums !! indN
                indS' = indS + num - 1

                springs' = sublist indS indS' springs

        at i = springs !! i

sublist :: Int -> Int -> [a] -> [a]
sublist i0 i1 = take (i1 - i0 + 1) . drop i0

-- |
-- >>> :main
-- 7771
-- 10861030975833
main :: IO ()
main = do
    input <- readParsedLines 2023 12 parser

    let unfolded = map (bimap (intercalate "?" . replicate 5) (concat . replicate 5)) input
        total    = sum . map (uncurry countArrangements)

    print $ total input
    print $ total unfolded
