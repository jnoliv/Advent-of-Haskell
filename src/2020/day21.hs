{-# Language OverloadedStrings, TupleSections #-}

import Advent.Megaparsec
import Advent.Utils (count)
import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (sort, sortBy, delete, intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

type AlgSet = S.Set String
type Algs   = M.Map String AlgSet

format :: Parser ([String], [String])
format = (,) <$> ings <* "(contains " <*> algs <* ")"
    where ings = many letterChar `endBy` " "
          algs = many letterChar `sepBy` ", "

mergeAlgsPossibilities :: [([String], [String])] -> Algs
mergeAlgsPossibilities = M.fromListWith S.intersection . concatMap toAlgSet
    where toAlgSet (i,a) = map (, S.fromList i) a

mapAlgsToIngs :: Algs -> [(String, String)]
mapAlgsToIngs = foldAlgs . sortByNIngs .  map (second S.toList) . M.toList
    where sortByNIngs = sortBy (compare `on` (length . snd))
          
          foldAlgs [] = []
          foldAlgs ((a,[i]) : algs) = (a,i) : (foldAlgs . sortByNIngs . map (second (delete i)) $ algs)

main :: IO()
main = do
    input <- readParsedLines 2020 21 format

    let algsPossibilities = mergeAlgsPossibilities input
        potencialAlgs     = M.foldr S.union S.empty algsPossibilities
        algsIngsMapping   = mapAlgsToIngs algsPossibilities

    print . count (`S.notMember` potencialAlgs) . concatMap fst $ input
    putStrLn . intercalate "," . map snd . sort $ algsIngsMapping
