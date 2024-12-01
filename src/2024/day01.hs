{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (readParsed, decimal, endBy, many, Parser)
import Advent.Utils (count)
import Data.List (transpose, sort)

parser :: Parser [[Int]]
parser = transpose <$> line `endBy` "\n"
    where
        line = (\a b -> [a,b]) <$> decimal <* many " " <*> decimal

-- |
-- >>> :main
-- 1938424
-- 22014209
main :: IO ()
main = do
    [l1, l2] <- map sort <$> readParsed 2024 1 parser       

    print . sum . map abs $ zipWith (-) l1 l2
    print . sum . zipWith (*) l1 $ map (flip count l2 . (==)) l1