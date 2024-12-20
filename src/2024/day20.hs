{-# LANGUAGE ImportQualifiedPost, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (distance, neighbours4, Coord)
import Advent.Utils (readAsMap)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

getDistances :: Map Coord Char -> Coord -> Coord -> Map Coord Int
getDistances track start end = go start (Map.size track) Map.empty
    where
        go pos distance distances
            | pos == end        = distances'
            | length nexts /= 1 = error ("expected 1 path but have " ++ show (length nexts))
            | otherwise         = go (head nexts) distance' distances'
            where
                distance'  = distance - 1
                distances' = Map.insert pos distance' distances

                nexts = filter ((&&) <$> (`Map.member` track) <*> (`Map.notMember` distances)) $ neighbours4 pos

getCheats :: Map Coord Int -> Int -> [(Coord,Coord,Int)]
getCheats distances n = map withSavings . concatMap getCheats $ Map.keys distances
    where
        getDist = fromJust . (`Map.lookup` distances)

        getCheats :: Coord -> [(Coord,Coord)]
        getCheats pos = map (pos,) . Map.keys $ Map.filterWithKey f distances
            where
                f k d = k /= pos && pos `distance` k <= n && d < getDist pos - 2

        withSavings :: (Coord,Coord) -> (Coord,Coord,Int)
        withSavings (start,end) = (start, end, getDist start - getDist end - start `distance` end)

-- |
-- >>> :main
-- 1518
-- 1032257
main :: IO ()
main = do
    track <- readAsMap (\c -> if c == '#' then Nothing else Just c) <$> readInputDefaults 2024 20

    let start = head . Map.keys $ Map.filter (== 'S') track
        end   = head . Map.keys $ Map.filter (== 'E') track

        distances = getDistances track start end

        savesAtLeast n (_,_,c) = c >= n

    mapM_ (print . length . filter (savesAtLeast 100) . getCheats distances) [2, 20]
