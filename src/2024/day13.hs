{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord)
import Advent.Math.LinearEquations (solve)
import Advent.Megaparsec (decimal, oneOf, Parser, readParsed, sepBy)
import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes)

type Machine  = (Coord, Coord, Coord)
type Solution = [Integer] --(Integer, Integer)

parser :: Parser [Machine]
parser = machine `sepBy` "\n"
    where
        machine = (,,) <$> button <*> button <*> prize
        button  = (,)  <$> ("Button " *> oneOf ['A', 'B'] *> ": " *>
                            "X+" *> decimal <* ", ")
                       <*> ("Y+" *> decimal <* "\n")
        prize   = (,)  <$> ("Prize: " *>
                            "X=" *> decimal <* ", ")
                       <*> ("Y=" *> decimal <* "\n")

minTokens :: [Machine] -> IO Integer
minTokens = fmap (sum . map tokens . filter (not . null)) . mapM findMoves
    where
        findMoves ((xA,yA), (xB,yB), (xP,yP)) = solve [[xA,xB],[yA,yB]] [xP,yP]

        tokens [a,b] = 3*a + b

-- |
-- >>> :main
-- 28059
-- 102255878088512
main :: IO ()
main = do
    input <- readParsed 2024 13 parser

    let err     = 10000000000000
        input2  = map (\(x,y,p) -> (x,y, bimap (+err) (+err) p)) input

    tokens1 <- minTokens input
    tokens2 <- minTokens input2

    print tokens1
    print tokens2
