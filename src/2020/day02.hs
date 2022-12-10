{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec
import Advent.Utils (xor, count)

type PasswordData = ((Int, Int), Char, String)

passwordFormat :: Parser PasswordData
passwordFormat = 
    (,,) <$> ((,) <$> decimal <* char '-' <*> decimal <* char ' ')
         <*> letterChar <* ": "
         <*> many letterChar

-- | Check if 'pass' has a count of characters 'c' between 'l' and 'r'
isValid1 :: PasswordData -> Bool
isValid1 ((l, r), c, pass) = l <= cnt && cnt <= r
    where cnt = count (c ==) pass

-- | Check if 'pass' has character 'c' at position 'p1' xor 'p2'
isValid2 :: PasswordData -> Bool
isValid2 ((p1, p2), c, pass) = (c1 == c) `xor` (c2 == c)
    where c1 = pass !! p1
          c2 = pass !! p2

-- |
-- >>> :main
-- 550
-- 634
main :: IO ()
main = do
    input <- readParsedLines 2020 2 passwordFormat

    let input2 = map (\((p1, p2), c, p) -> ((pred p1, pred p2), c, p)) input

    print . count isValid1 $ input
    print . count isValid2 $ input2
