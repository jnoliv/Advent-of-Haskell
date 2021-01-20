{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

type Chemical = String
type Molecule = [Chemical]

format :: Parser ([(Chemical, Molecule)], Molecule)
format = (,) <$> (replacement `endBy` "\n") <* "\n"
             <*> some chemical
    where
        replacement = (,) <$> some letterChar <* " => " <*> some chemical
        chemical    = try ((\c1 c2 -> [c1,c2]) <$> upperChar <*> lowerChar)
                      <|>  return              <$> upperChar

calibrate :: Map Chemical [Molecule] -> Seq Chemical -> Set Molecule
calibrate repls formula = go Seq.empty formula Set.empty
    where
        go :: Seq Chemical -> Seq Chemical -> Set Molecule -> Set Molecule
        go prev (chem Seq.:<| after) set = go (prev Seq.:|> chem) after $ foldr Set.insert set molecules
            where molecules = [makeMol prev chem' after | chem' <- Map.findWithDefault [] chem repls]
        go _    _                    set = set

        makeMol :: Seq Chemical -> Molecule -> Seq Chemical -> Molecule
        makeMol prev mol after = toList (prev Seq.>< molSeq Seq.>< after)
            where molSeq = Seq.fromList mol

-- |
-- >>> :main
-- 576
main :: IO ()
main = do
    (repls', formula') <- readParsed 2015 19 format

    let repls   = Map.fromListWith (++) $ map (second return) repls'
        formula = Seq.fromList formula'

    print . Set.size $ calibrate repls formula

    --print repls
    --print formula

    --print $ calibrate repls formula
