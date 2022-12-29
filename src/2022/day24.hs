{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid
import Advent.Utils

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type Limits = ((Int,Int),(Int,Int))

data Blizz = BlizzU | BlizzR | BlizzD | BlizzL | You | Empty
    deriving (Eq, Ord)

readBlizz :: Char -> Maybe Blizz
readBlizz '^' = Just BlizzU
readBlizz '>' = Just BlizzR
readBlizz 'v' = Just BlizzD
readBlizz '<' = Just BlizzL
readBlizz '.' = Just Empty
readBlizz  _  = Nothing

moveBlizz :: Limits -> (Coord, Blizz) -> (Coord, Blizz)
moveBlizz ((y0,y1), _) (c, BlizzU) = let (y,x) = up    c in if y < y0 then ((y1,x), BlizzU) else ((y,x), BlizzU)
moveBlizz ((y0,y1), _) (c, BlizzD) = let (y,x) = down  c in if y > y1 then ((y0,x), BlizzD) else ((y,x), BlizzD)
moveBlizz (_, (x0,x1)) (c, BlizzR) = let (y,x) = right c in if x > x1 then ((y,x0), BlizzR) else ((y,x), BlizzR)
moveBlizz (_, (x0,x1)) (c, BlizzL) = let (y,x) = left  c in if x < x0 then ((y,x1), BlizzL) else ((y,x), BlizzL)

minimise :: Set Coord -> Limits -> [(Coord, Blizz)] -> Coord -> Coord -> (Int, Int)
minimise tiles limits blizzs0 start end = (n1, n3)
    where
        (n1, _, blizzs1) = until (hasGoal end)   moveAll (0,  Set.singleton start, blizzs0)
        (n2, _, blizzs2) = until (hasGoal start) moveAll (n1, Set.singleton end,   blizzs1)
        (n3, _, _)       = until (hasGoal end)   moveAll (n2, Set.singleton start, blizzs2)

        hasGoal goal (_, s, _) = goal `Set.member` s

        validMoves :: Set Coord -> Coord -> [Coord] 
        validMoves blizzs = filter valid . sequence playerMoves
            where
                playerMoves = [up, right, down, left, id]

                valid p = p `Set.member` tiles && p `Set.notMember` blizzs

        moveAll :: (Int, Set Coord, [(Coord, Blizz)]) -> (Int, Set Coord, [(Coord, Blizz)])
        moveAll (n, positions, blizzs) = (n + 1, positions', blizzs')
            where
                blizzs' = map (moveBlizz limits) blizzs
                asSet   = Set.fromList $ map fst blizzs'

                positions' = Set.fromList . concatMap (validMoves asSet) $ Set.toList positions

-- |
-- >>> :main
-- 281
-- 807
main :: IO ()
main = do
    area <- readAsMap readBlizz <$> readInputDefaults 2022 24

    let ((y0,x0), (y1,x1)) = findDefininingPoints $ Map.keys area

        limits    = ((y0 + 1, y1 - 1), (x0, x1))
        tiles     = Set.fromList $ Map.keys area
        blizzards = Map.toList $ Map.filter (/= Empty) area

        (trip1, trip3) = minimise tiles limits blizzards (y0,x0) (y1,x1)

    print $ trip1
    print $ trip3
