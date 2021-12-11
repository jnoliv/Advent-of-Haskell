import Advent.API (readInputDefaults)
import Advent.Utils (readAsSet)
import qualified Data.Set as Set

type Trees = (Int, Int, Set.Set (Int,Int))  -- (#rows, #cols, tree positions)

parseInput :: String -> Trees
parseInput input = (length ls, length (head ls), trees)
    where trees = readAsSet (== '#') input
          ls    = lines input

-- | Count all the trees hit by following the given slope
countTrees :: Trees -> (Int, Int) -> Int
countTrees trees@(nRows,_,_) (hStep, vStep) =
    length . filter (hasTree trees) $ zip [0,vStep..nRows] [0,hStep..] 

-- | Check if the given coordinates are a tree
hasTree :: Trees -> (Int,Int) -> Bool
hasTree (_, nCols, trees) (x,y) = Set.member (x,y') trees
    where y' = y `mod` nCols

-- |
-- >>> :main
-- 156
-- 3521829480
main :: IO()
main = do
    trees <- parseInput <$> readInputDefaults 2020 3

    let slopes   = [(1,1), (3,1), (5,1), (7,1), (1,2)]
        hitTrees = map (countTrees trees) slopes
    
    print $ hitTrees !! 1
    print $ product hitTrees
