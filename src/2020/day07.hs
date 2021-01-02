import AdventAPI
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import qualified Data.IntSet as Set

type AdjList  = [[(Int, Int)]]  -- (node, edge weight)
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
-- have their neighbours presented. Example input line:
-- "dim tan bags contain 3 shiny teal bags, 5 bright white bags, 4 striped bronze bags."
parseInput' :: [[String]] -> ColorMap -> AdjList
parseInput' (n:ns) colorMap = (if n !! 4 /= "no" then neighbours else []) : parseInput' ns colorMap
    where toWeightedNeigh (w:c1:c2:_) = (fromJust $ Map.lookup (c1 ++ c2) colorMap, read w)
          neighbours      = map toWeightedNeigh . chunksOf 4 . drop 4 $ n
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
          ns'      = ns ++ filter (`Set.notMember` visited) (map fst $ adj !! n)
reachable _ visited [] = visited

-- | Calculate the weight of all paths starting from 'current'. For
-- the problem in question, each node weighs 1
weightAllPaths :: AdjList -> Int -> Int
weightAllPaths adj current = 1 + sum (map edgeWeight $ adj !! current)
    where edgeWeight (n,w) = w * weightAllPaths adj n

-- |
-- >>> :main
-- 226
-- 9569
main :: IO()
main = do
    contents <- readInputDefaults 2020 7

    let (colorMap, adj) = parseInput contents
        reachableFrom   = map (reachable adj Set.empty . map fst) adj
        shinygoldID     = fromJust $ Map.lookup "shinygold" colorMap
        canReach        = map (Set.member shinygoldID) reachableFrom

    print . length . filter id $ canReach

    -- This prints False, there are no loops. Phewww...
    -- print . or $ zipWith Set.member [0..] reachableFrom

    print . pred $ weightAllPaths adj shinygoldID
