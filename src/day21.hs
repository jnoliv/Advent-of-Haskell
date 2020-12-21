{-# Language OverloadedStrings, TupleSections #-}

import Control.Applicative (many)
import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (sortBy, delete, intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import Utils (Parser, readParsedLines, count)
import Text.Megaparsec (endBy, sepBy)
import Text.Megaparsec.Char (letterChar)

type AlgSet = S.Set String
type Algs   = M.Map String AlgSet

format :: Parser ([String], [String])
format = (,) <$> ings <* "(contains " <*> algs <* ")"
    where ings = many letterChar `endBy` " "
          algs = many letterChar `sepBy` ", "

mergeAlgsPossibilities :: [([String], [String])] -> Algs
mergeAlgsPossibilities = foldl foldFunc M.empty . concatMap toAlgSet
    where foldFunc acc (k,v) = M.insertWith S.intersection k v acc
          toAlgSet (i,a)     = map (, S.fromList i) a

mapAlgsToIngs :: Algs -> [(String, String)]
mapAlgsToIngs = sortBy (compare `on` fst) . foldAlgs . sortByNIngs .  map (second S.toList) . M.toList
    where sortByNIngs = sortBy (compare `on` (length . snd))
          
          foldAlgs [] = []
          foldAlgs ((a,[i]) : algs) = (a,i) : (foldAlgs . sortByNIngs . map (second (delete i)) $ algs)

main :: IO()
main = do
    input <- readParsedLines 21 format

    let allIngredients    = concatMap fst input
        uniqueIngredients = S.fromList allIngredients
        algsPossibilities = mergeAlgsPossibilities input
        
        potencialAlgs     = M.foldr S.union S.empty algsPossibilities
        safeIngredients   = uniqueIngredients S.\\ potencialAlgs

        algsIngsMapping   = mapAlgsToIngs algsPossibilities

    print . count (`S.member` safeIngredients) $ allIngredients
    putStrLn . intercalate "," . map snd $ algsIngsMapping
