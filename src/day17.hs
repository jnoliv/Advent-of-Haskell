import AdventAPI (readInputDefaults)
import qualified Data.Set as S
import Utils (readAsSet)

type Coord3 = (Int,Int,Int)

getNeighbours :: Coord3 -> [Coord3]
getNeighbours (x,y,z) =
    [(x + dx, y + dy, z + dz) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dx /= 0 || dy /= 0 || dz /= 0]

countActiveNeighbours :: S.Set Coord3 -> Coord3 -> Int
countActiveNeighbours set = length . filter (`S.member` set) . getNeighbours

step :: S.Set Coord3 -> S.Set Coord3
step set = S.union new kept
    where neighbours         = S.foldr (\pos set' -> S.union set' . S.fromList $ getNeighbours pos) S.empty set
          inactiveNeighbours = S.filter (`S.notMember` set) neighbours
          new                = S.filter ((== 3) . countActiveNeighbours set) inactiveNeighbours
          kept               = S.filter ((`elem` [2,3]) . countActiveNeighbours set) set

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 17

    let game   = S.map (\(x,y) -> (x,y,0)) $ readAsSet (== '#') contents
        cycle6 = (!! 6) . iterate step $ game

    print $ S.size cycle6
