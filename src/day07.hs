import qualified Common.AdventAPI as AdventAPI

import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import qualified Data.IntSet as Set

type AdjList  = [[Int]]
type ColorMap = Map.Map String Int

-- | Parse the human readable adjacency list into a more
-- machine friendly list
parseInput :: String -> (ColorMap, AdjList)
parseInput input = ( colorMap, parseInput' splitInput colorMap)
    where splitInput = map words . lines $ input
          colorMap   = makeColorMap splitInput

-- | Recursively build the list by converting each line to a list
-- of neighbours (using their integer ID). This creates the adjacency
-- list because 'makeColorMap' assigns IDs to colors in the order they
-- have their neighbours presented
parseInput' :: [[String]] -> ColorMap -> AdjList
parseInput' (n:ns) colorMap = (if n !! 4 /= "no" then neighboursInt else []) : parseInput' ns colorMap
    where neighbours    = map (concat . take 2 . tail) . chunksOf 4 . drop 4 $ n
          neighboursInt = map (fromJust . flip Map.lookup colorMap) neighbours
parseInput' [] _ = [] 

-- | Make a map from all colors to their corresponding integer ID
makeColorMap :: [[String]] -> ColorMap
makeColorMap = foldl f Map.empty
    where f m n = Map.insert (concat . take 2 $ n) (Map.size m) m

-- | Build the set of all reachable nodes from the first node in
-- the given list. Basically a BFS where the visited set is the return
reachable :: AdjList -> Set.IntSet -> [Int] -> Set.IntSet
reachable adj visited (n:ns) = reachable adj visited' ns'
    where visited' = Set.insert n visited
          ns'      = ns ++ filter (`Set.notMember` visited) (adj !! n)
reachable _ visited [] = visited

main :: IO()
main = do
    contents <- AdventAPI.readInput 7 "../session-cookie.txt" "../input"

    let (colorMap, adj) = parseInput contents
        reachableFrom   = map (reachable adj Set.empty . (: [])) [0 .. length adj - 1]
        canReach        = map (Set.member . fromJust . Map.lookup "shinygold" $ colorMap) reachableFrom

    print . pred . length . filter id $ canReach
