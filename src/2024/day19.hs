{-# LANGUAGE OverloadedStrings, BlockArguments, LambdaCase #-}

import Advent.Megaparsec (readParsed, letterChar, endBy, many, sepBy, Parser)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.MemoTrie (memo2)

parser :: Parser ([String], [String])
parser = (,) <$> patterns <* "\n\n" <*> designs
    where
        patterns = many letterChar `sepBy` ", "
        designs  = many letterChar `endBy` "\n"


isPossible :: [String] -> String -> Bool
-- Could use countPossible for both parts, but this is faster for part 1.
isPossible = memo2 $ \cases
        patterns []     -> True
        patterns design -> any (isPossible patterns) $ mapMaybe (matchPrefix design) patterns

countPossible :: [String] -> String -> Int
countPossible = memo2 $ \cases
        patterns []     -> 1
        patterns design -> sum . map (countPossible patterns) $ mapMaybe (matchPrefix design) patterns

matchPrefix :: String -> String -> Maybe String
matchPrefix design pattern
    | pattern `isPrefixOf` design = Just $ drop (length pattern) design
    | otherwise                   = Nothing

-- |
-- >>> :main
-- 367
-- 724388733465031
main :: IO ()
main = do
    (patterns, designs) <- readParsed 2024 19 parser

    print . length $ filter (isPossible patterns) designs
    print . sum $ map (countPossible patterns) designs
