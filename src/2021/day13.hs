{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (Parser, readParsed, letterChar, decimal, endBy1)
import Advent.Coord.Grid (Coord)
import Advent.Utils (showSet)
import Data.Bifunctor (first, second)
import Data.Set (Set)
import Data.Set qualified as Set

type Paper = Set Coord
type Fold  = (Char, Int)

format :: Parser ([Coord], [Fold])
format = (,) <$> dots <* "\n" <*> folds
    where
        dots  = dot `endBy1` "\n"
        dot   = flip (,) <$> decimal <* "," <*> decimal
        folds = fold `endBy1` "\n"
        fold  = (,) <$> ("fold along " *> letterChar <* "=") <*> decimal 

-- |
-- >>> :main
-- 724
--  ##  ###    ## ###  #### ###  #  # #   
-- #  # #  #    # #  # #    #  # #  # #   
-- #    #  #    # ###  ###  #  # #  # #   
-- #    ###     # #  # #    ###  #  # #   
-- #  # #    #  # #  # #    # #  #  # #   
--  ##  #     ##  ###  #### #  #  ##  ####
main :: IO ()
main = do
    (dots, folds) <- readParsed 2021 13 format

    let paper  = Set.fromList dots
        folded = foldl fold paper folds

    print . Set.size $ fold paper (head folds)

    putStrLn $ showSet (\b -> if b then '#' else ' ') folded

fold :: Paper -> Fold -> Paper
fold s (c, v)
    | c == 'x' = Set.map (second (f v)) s
    | c == 'y' = Set.map (first  (f v)) s
    where
        f v n = if n > v then v - (n - v) else n
