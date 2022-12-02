{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (Parser, parseLines, letterChar)

data HandShape = Rock | Paper | Scissors
data Outcome = Win | Draw | Loss

format :: (Char -> a) -> Parser (HandShape, a)
format p = (,) <$> (parsePlay <$> letterChar) 
               <*  " "
               <*> (p         <$> letterChar)

parsePlay :: Char -> HandShape
parsePlay c
    | c == 'A' || c == 'X' = Rock
    | c == 'B' || c == 'Y' = Paper
    | c == 'C' || c == 'Z' = Scissors

parseOutcome :: Char -> Outcome
parseOutcome 'X' = Loss
parseOutcome 'Y' = Draw
parseOutcome 'Z' = Win

outcome :: (HandShape, HandShape) -> Outcome
outcome (Rock    , Scissors) = Loss
outcome (Rock    , Paper   ) = Win
outcome (Paper   , Rock    ) = Loss
outcome (Paper   , Scissors) = Win
outcome (Scissors, Rock    ) = Win
outcome (Scissors, Paper   ) = Loss
outcome (_       , _       ) = Draw

toShapesRound :: (HandShape, Outcome) -> (HandShape, HandShape)
toShapesRound (Rock,     Win)  = (Rock,     Paper   )
toShapesRound (Rock,     Draw) = (Rock,     Rock    )
toShapesRound (Rock,     Loss) = (Rock,     Scissors)
toShapesRound (Paper,    Win)  = (Paper,    Scissors)
toShapesRound (Paper,    Draw) = (Paper,    Paper   )
toShapesRound (Paper,    Loss) = (Paper,    Rock    )
toShapesRound (Scissors, Win)  = (Scissors, Rock    )
toShapesRound (Scissors, Draw) = (Scissors, Scissors)
toShapesRound (Scissors, Loss) = (Scissors, Paper   )

shapeScore :: HandShape -> Integer
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

outcomeScore :: Outcome -> Integer
outcomeScore Win  = 6
outcomeScore Draw = 3
outcomeScore Loss = 0

roundScore :: (HandShape, HandShape) -> Integer
roundScore (elf, mine) = outcomeScore o + shapeScore mine
    where o = outcome (elf, mine)

-- |
-- >>> :main
-- 13682
-- 12881
main :: IO ()
main = do
    input <- readInputDefaults 2022 2

    let parsedInput1 = parseLines (format parsePlay)   input
        parsedInput2 = parseLines (format parseOutcome) input

        totalScore1 = sum . map  roundScore                  $ parsedInput1
        totalScore2 = sum . map (roundScore . toShapesRound) $ parsedInput2

    print totalScore1
    print totalScore2
