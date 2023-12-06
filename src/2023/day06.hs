{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (sepBy, some, digitChar, readParsed, Parser)
import Control.Lens (over, both)

parser :: Parser ([String],[String])
parser = (,) <$> (("Time:"     *> some " ") *> some digitChar `sepBy` some " ") <* "\n"
             <*> (("Distance:" *> some " ") *> some digitChar `sepBy` some " ") <* "\n"

margin :: [(Int,Int)] -> Int
margin = product . map winOptions

winOptions :: (Int, Int) -> Int
winOptions (t, d) = length $filter (> d) [x * (t - x) | x <- [1..t]]

-- |
-- >>> :main
-- 1195150
-- 42550411
main :: IO ()
main = do
    input1 <- uncurry zip . over both (map read) <$> readParsed 2023 6 parser
    input2 <- over both (read . concat)          <$> readParsed 2023 6 parser

    print $ margin input1
    print $ winOptions input2
