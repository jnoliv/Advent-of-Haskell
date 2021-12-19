{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (decimal, endBy1, sepBy1, readParsed, sdecimal, Parser)
import Advent.Coord.Grid3 (Point, orientations, (.-), (.+), manhattan)
import Advent.Utils (combinations)
import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set

type Map = Set Point

nothing = (Set.empty,(0,0,0))

format :: Parser [Map]
format = scanner `sepBy1` "\n"
    where
        scanner = Set.fromList <$> (banner *> (point `endBy1` "\n"))
        banner  = "--- scanner " *> decimal <* " ---\n"
        point   = (,,) <$> sdecimal <* ","
                       <*> sdecimal <* ","
                       <*> sdecimal

-- |
-- >>> :main
-- 414
-- 13000
main :: IO ()
main = do
    scanners <- readParsed 2021 19 format

    let (beacons, scannerPositions) = match scanners []

        scannerPairs = combinations 2 scannerPositions
        distances    = map (\[a,b] -> manhattan a b) scannerPairs

    print . Set.size $ beacons
    print $ maximum distances

match :: [Map] -> [Point] -> (Map, [Point])
match [base]      scannersPositions = (base, scannersPositions)
match (base:rest) scannersPositions = match (base' : rest') (p : scannersPositions)
    where
        ((base', p), rest') = tryMatchPairs base [] rest

tryMatchPairs :: Map -> [Map] -> [Map] -> ((Map, Point), [Map])
tryMatchPairs base prev (cur : nexts)
    | matches   = ((Set.union base points, p), prev ++ nexts)
    | otherwise = tryMatchPairs base (prev ++ [cur]) nexts
    where
        (matches, (points, p)) = tryMatchPair base $ orientations cur

tryMatchPair :: Map -> [Map] -> (Bool, (Map, Point))
tryMatchPair _    []            = (False, nothing)
tryMatchPair base (cur : nexts)
    | matches   = (matches, points)
    | otherwise = tryMatchPair base nexts
    where
        (matches, points) = tryMatchOrientations base cur

tryMatchOrientations :: Map -> Map -> (Bool, (Map, Point))
tryMatchOrientations base other = case maybeMatch of
    Just v  -> (True, v)
    Nothing -> (False, nothing)
    where
        translateOnEachPoint = [translate baseP otherP other | baseP <- Set.toList base, otherP <- Set.toList other]
        translate o d        = let vec = (o .- d) in (\m -> (Set.map (vec .+) m, vec))

        match12 m1 m2 = (>= 12) . Set.size $ Set.intersection m1 m2

        maybeMatch    = find (match12 base . fst) translateOnEachPoint
