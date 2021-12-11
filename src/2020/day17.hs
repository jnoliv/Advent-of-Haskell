import Advent.API (readInputDefaults)
import Advent.Life
import Advent.Utils (readAsSet)
import Data.List (delete)
import qualified Data.Set as S

type CoordN = [Int]

neighbours :: CoordN -> [CoordN]
neighbours p = delete p . traverse (\c -> [c - 1, c, c + 1]) $ p

rules :: Bool -> Int -> Bool
rules True  n = n == 2 || n == 3
rules False n = n == 3

-- |
-- >>> :main
-- 257
-- 2532
main :: IO()
main = do
    contents <- readInputDefaults 2020 17

    let slice       = readAsSet (== '#') contents
        step        = evolve neighbours rules

        game        = S.map (\(x,y) -> [x,y,0]) slice
        cycle6      = (!! 6) . iterate step $ game

        hyperGame   = S.map (\(x,y) -> [x,y,0,0]) slice
        hyperCycle6 = (!! 6) . iterate step $ hyperGame

    print . countAlive $ cycle6
    print . countAlive $ hyperCycle6
