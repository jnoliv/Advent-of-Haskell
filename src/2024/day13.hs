{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord)
import Advent.Megaparsec (decimal, oneOf, Parser, readParsed, sepBy)
import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes)
import Z3.Monad

type Machine  = (Coord, Coord, Coord)
type Solution = (Integer, Integer)

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

getSolutions :: AST -> AST -> Z3 [Solution]
getSolutions a b = go []
  where go acc = do
          maybeSolution <- getSolution
          case maybeSolution of
               Nothing      -> return acc
               Just s@[a',b'] -> do restrictSolution s
                                    go ((a',b') : acc)

        restrictSolution [sA,sB] =
          assert =<< mkNot =<< mkOr =<< sequence [
            mkEq a =<< mkIntNum sA,
            mkEq b =<< mkIntNum sB]

        getSolution = fmap snd $ withModel $ \m ->
          catMaybes <$> mapM (evalInt m) [a,b]

script :: Machine -> Z3 [Solution]
script ((xA,yA), (xB,yB), (xP,yP)) = do
    -- a * xA + b * xB = xP
    -- a * yA + b * yB = yP

    a   <- mkFreshIntVar "a"
    _xA <- mkIntNum xA
    _yA <- mkIntNum yA

    b   <- mkFreshIntVar "b"
    _xB <- mkIntNum xB
    _yB <- mkIntNum yB

    _xP <- mkIntNum xP
    _yP <- mkIntNum yP

    assert =<< mkEq _xP =<< mkAdd =<< sequence [
        mkMul [a, _xA],
        mkMul [b, _xB]]

    assert =<< mkEq _yP =<< mkAdd =<< sequence [
        mkMul [a, _yA],
        mkMul [b, _yB]]

    getSolutions a b

minTokens :: [Machine] -> IO Integer
minTokens = fmap (sum . map (minimum . map tokens) . filter (not . null)) . mapM (evalZ3 . script)
    where
        tokens (a,b) = 3*a + b

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
