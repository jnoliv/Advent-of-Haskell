{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (readParsed, decimal, endBy, sepBy, Parser)
import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (sortBy)

import Data.IntMap (IntMap, (!))
import Data.IntMap qualified as Map

parser :: Parser ([(Int,Int)], [[Int]])
parser = (,) <$> rules <* "\n" <*> updates
    where
        rules   = rule `endBy` "\n"
        rule    = (,) <$> decimal <* "|" <*> decimal
        updates = update `endBy` "\n"
        update  = decimal `sepBy` ","

isOrdered :: IntMap [Int] -> [Int] -> Bool
isOrdered rules update = all satisfiesRules updateI
    where
        updateI  = zip update [0..]
        indexMap = Map.fromList updateI

        satisfiesRules (page,i) = all (i <) pageRulesI
            where
                pageRules  = Map.findWithDefault [] page rules
                pageRulesI = map (indexMap !) . filter (`Map.member` indexMap) $ pageRules

middle :: [a] -> a
middle l = l !! (length l `div` 2)

sortPages :: IntMap [Int] -> [Int] -> [Int]
sortPages rules update = map fst sortedRules
    where
        relevantRules = filter ((`elem` update) . fst) (Map.toList rules)
        filteredRules = map (second (filter (`elem` update))) relevantRules
        sortedRules   = sortBy (flip compare `on` length . snd) filteredRules

-- |
-- >>> :main
-- 6041
-- 4884
main :: IO ()
main = do
    (rules', updates) <- readParsed 2024 5 parser

    let rules = Map.fromListWith (++) (map (second (:[])) rules')

    print . sum . map middle . filter (isOrdered rules) $ updates
    print . sum . map (middle . sortPages rules) . filter (not . isOrdered rules) $ updates
