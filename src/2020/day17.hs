import AdventAPI (readInputDefaults)
import Advent.Utils (readAsSet)
import Data.List (delete)
import qualified Data.Set as S

type CoordN = [Int]

getNeighbours :: CoordN -> [CoordN]
getNeighbours p = delete p . traverse (\c -> [c - 1, c, c + 1]) $ p

countActiveNeighbours :: S.Set CoordN -> CoordN -> Int
countActiveNeighbours set = length . filter (`S.member` set) . getNeighbours

step :: S.Set CoordN -> S.Set CoordN
step set = S.union new kept
    where neighbours         = S.foldr (\pos set' -> S.union set' . S.fromList $ getNeighbours pos) S.empty set
          inactiveNeighbours = S.filter (`S.notMember` set) neighbours
          new                = S.filter ((== 3) . countActiveNeighbours set) inactiveNeighbours
          kept               = S.filter ((`elem` [2,3]) . countActiveNeighbours set) set

main :: IO()
main = do
    contents <- readInputDefaults 2020 17

    let slice       = readAsSet (== '#') contents

        game        = S.map (\(x,y) -> [x,y,0]) slice
        cycle6      = (!! 6) . iterate step $ game

        hyperGame   = S.map (\(x,y) -> [x,y,0,0]) slice
        hyperCycle6 = (!! 6) . iterate step $ hyperGame

    print $ S.size cycle6
    print $ S.size hyperCycle6
