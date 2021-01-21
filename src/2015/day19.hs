{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Data.Bifunctor (second)
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

type Chemical = String
type Molecule = Seq Chemical
type ReplMap  = Map Chemical [Molecule]

format :: Parser ([(Chemical, [Chemical])], [Chemical])
format = (,) <$> (replacement `endBy` "\n") <* "\n"
             <*> some chemical
    where
        replacement = (,) <$> chemical <* " => " <*> some chemical
        chemical    = try ((\a b -> [a,b]) <$> upperChar <*> lowerChar)
                      <|>  return          <$> letterChar

calibrate :: ReplMap -> Molecule -> Set Molecule
calibrate repls medicine = go Seq.empty medicine Set.empty
    where
        go :: Molecule -> Molecule -> Set Molecule -> Set Molecule
        go prev (chem Seq.:<| after) set =
            go (prev Seq.:|> chem) after $ foldr Set.insert set molecules
            where
                molecules = [prev Seq.>< mol Seq.>< after | mol <- Map.findWithDefault [] chem repls]
        go _ _ set = set

fabricate :: ReplMap -> [Chemical] -> Molecule -> Molecule -> Maybe Int
fabricate repls terminals medicine start = go (Set.singleton start) 0
    where
        size      = Seq.length medicine
        goalTerms = map (\c -> count (== c) medicine) terminals

        eligible :: Molecule -> Bool
        eligible mol = Seq.length mol <= size && and (zipWith (<=) nTerms goalTerms)
            where nTerms = map (\c -> count (== c) mol) terminals

        count :: (a -> Bool) -> Seq a -> Int
        count cond = Seq.length . Seq.filter cond

        go :: Set Molecule -> Int -> Maybe Int
        go set steps
            | Set.null set              = Nothing
            | medicine `Set.member` set = Just steps
            | otherwise                 = go newMols (steps + 1)
            where
                newMols      = Set.filter eligible . Set.unions . map (calibrate repls) . Set.toList $ set

-- |
-- >>> :main
-- 576
main :: IO ()
main = do
    (repls', medicine') <- readParsed 2015 19 format

    let repls     = Map.fromListWith (++) $ map (second (return . Seq.fromList)) repls'
        medicine  = Seq.fromList medicine'

        terminals = filter (`Map.notMember` repls) . nub $ medicine'

    print . Set.size $ calibrate repls medicine
    print            $ fabricate repls terminals medicine (Seq.singleton "e")