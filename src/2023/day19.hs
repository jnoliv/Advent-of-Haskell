{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (letterChar, decimal, endBy, sepBy, some, (<|>), try, symbolChar, readParsed, Parser)
import Data.Maybe (fromJust, isJust)

import Data.Map (Map)
import Data.Map qualified as Map

type Workflows = Map String Workflow
type Workflow  = [Rule]
data Rule      = JmpIf Char Char Int String | Jmp String
type Op        = Int -> Int -> Bool

type Part      = [Int]   -- [x, m, a, s]

type Ranges    = [Range] -- [x, m, a, s]
type Range     = (Int,Int)

getValue :: Char -> [a] -> a
getValue 'x' = (!! 0)
getValue 'm' = (!! 1)
getValue 'a' = (!! 2)
getValue 's' = (!! 3)

setValue :: Char -> a -> [a] -> [a]
setValue 'x' n [a,b,c,d] = [n,b,c,d]
setValue 'm' n [a,b,c,d] = [a,n,c,d]
setValue 'a' n [a,b,c,d] = [a,b,n,d]
setValue 's' n [a,b,c,d] = [a,b,c,n]

getOp :: Char -> (Int -> Int -> Bool)
getOp '<' = (<)
getOp '>' = (>)

parser :: Parser (Workflows, [Part])
parser = (,) <$> workflows <* "\n" <*> parts
    where
        workflows = Map.fromList <$> workflow `endBy` "\n"
        workflow  = (,) <$> some letterChar
                        <*  "{" <*> rule `sepBy` "," <* "}"
        rule      = try (JmpIf <$> letterChar <*> symbolChar <*> decimal <* ":" <*> some letterChar)
                    <|> (Jmp   <$> some letterChar)

        parts = part `endBy` "\n"
        part  = "{" *> value `sepBy` "," <* "}"
        value = letterChar *> "=" *> decimal

sortPart :: Workflows -> Part -> Bool
sortPart workflows part = sortPart' "in"
    where
        sortPart'  "A" = True
        sortPart'  "R" = False
        sortPart' name = sortPart' $ applyWorkflow (workflows Map.! name)

        applyWorkflow (rule : rules) = case rule of
            Jmp           name -> name
            JmpIf id op n name -> if getValue id part `op'` n
                                    then name
                                    else applyWorkflow rules
                where
                    op' = getOp op

countAccepted :: Workflows -> Int
countAccepted workflows = sortRange "in" (replicate 4 (1,4000))
    where
        sortRange :: String -> Ranges -> Int
        sortRange  "A" ranges = product (map rangeLength ranges)
        sortRange  "R" ranges = 0
        sortRange name ranges
            | null ranges = 0
            | otherwise   = applyWorkflow ranges (workflows Map.! name)

        applyWorkflow :: Ranges -> Workflow -> Int
        applyWorkflow ranges (rule : rules) = case rule of
            Jmp           name -> sortRange name ranges
            JmpIf id op n name -> thisCount + nextCount
                where
                    (thisRange, nextRange) = splitRange op n (getValue id ranges)

                    thisCount = if isJust thisRange
                        then sortRange name $ setValue id (fromJust thisRange) ranges
                        else 0

                    nextCount = if isJust nextRange
                        then flip applyWorkflow rules $ setValue id (fromJust nextRange) ranges
                        else 0
                
        rangeLength :: Range -> Int
        rangeLength (a,b) = b - a + 1

        splitRange :: Char -> Int -> Range -> (Maybe Range, Maybe Range)
        splitRange '<' n (a,b)
            | n <  a    = (        Nothing, Just (a,b))
            | n <= b    = (Just (a, n - 1), Just (n,b))
            | otherwise = (Just (a,     b),    Nothing)
        splitRange '>' n (a,b)
            | b <  n    = (        Nothing, Just (a,b))
            | a <= n    = (Just (n + 1, b), Just (a,n))
            | otherwise = (Just (    a, b),    Nothing)

-- |
-- >>> :main
-- 425811
-- 131796824371749
main :: IO ()
main = do
    (workflows, parts) <- readParsed 2023 19 parser

    let accepted = filter (sortPart workflows) parts

    print . sum $ map sum accepted
    print $ countAccepted workflows
