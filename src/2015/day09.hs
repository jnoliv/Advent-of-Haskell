{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Data.List (nub)
import Data.Set (Set)
import Data.Set qualified as Set

import Debug.Trace (trace)

type CityPair = (String,String)
type Distance = (CityPair, Int)

format :: Parser Distance
format = (,) <$> ((,) <$> many letterChar <* " to "
                      <*> many letterChar <* " = ")
             <*> decimal

tsp :: ([Int] -> Int) -> [Distance] -> Int
tsp ordF dists = ordF . map (go <$> id <*> Set.singleton <*> const 0) $ cities
    where
        cities = nub . concatMap (\((a,b),_) -> [a, b]) $ dists

        go cur seen dist 
            | Set.size seen == length cities = dist
            | otherwise                      = ordF . map f $ filterUnseenAdj cur seen dists
            where
                f (city, curDist) = go city (Set.insert city seen) (dist + curDist)

filterUnseenAdj :: String -> Set String -> [Distance] -> [(String, Int)]
filterUnseenAdj city seen = filter ((`Set.notMember` seen) . fst) . map remCity . filter (hasCity)
    where hasCity ((a,b), _) = city == a || city == b
          remCity ((a,b), d) = if a /= city then (a, d) else (b, d)

-- |
-- >>> :main
-- 251
-- 898
main :: IO ()
main = do
    input <- readParsedLines 2015 9 format

    print . tsp minimum $ input
    print . tsp maximum $ input
