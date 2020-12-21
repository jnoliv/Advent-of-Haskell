{-# Language OverloadedStrings, TupleSections #-}

import Control.Applicative (many)
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

main :: IO()
main = do
    input <- readParsedLines 21 format

    let allIngredients    = concatMap fst input
        uniqueIngredients = S.fromList allIngredients
        algsPossibilities = mergeAlgsPossibilities input
        
        potencialAlgs     = M.foldr S.union S.empty algsPossibilities
        safeIngredients   = uniqueIngredients S.\\ potencialAlgs

    print . count (`S.member` safeIngredients) $ allIngredients
