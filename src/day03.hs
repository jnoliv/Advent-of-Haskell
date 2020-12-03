import qualified Common.AdventAPI as AdventAPI

import Control.Monad (forM_)
import qualified Data.Set as Set

type AreaMap = Set.Set (Int,Int)
type BoundedAreaMap = (Int,Int,AreaMap)

-- | Check if the given coordinates are a tree in the given area map
hasTree :: BoundedAreaMap -> (Int,Int) -> Bool
hasTree (_, nCols, areaMap) (x,y) =
    let newY = y `mod` nCols
    in Set.member (x,newY) areaMap 

-- | Fold a zipped input row into an area map
foldRow :: (Int, [(Int, Char)]) -> AreaMap
foldRow (rowNumber, row) = foldr foldFunc Set.empty row
    where foldFunc (col, c) set = if c == '#' then Set.insert (rowNumber, col) set else set

-- | Parse the input into a set of all positions with trees
parseInput :: String -> BoundedAreaMap
parseInput input = 
    let zipped = zip [0..] . map (zip [0..]) $ lines input
        areaMap = foldr (Set.union . foldRow) Set.empty zipped
    in (length zipped, length . snd . head $ zipped, areaMap)

-- | Count all the trees hit by following the given slope
countTrees :: BoundedAreaMap -> (Int, Int) -> Int
countTrees bAreaMap@(nRows,_,_) (hStep, vStep) =
    let positions = zip [0,vStep..nRows] [0,hStep..]
    in foldr (\pos cnt -> if hasTree bAreaMap pos then cnt + 1 else cnt) 0 positions

main :: IO()
main = do
    contents <- AdventAPI.readInput 3 "../session-cookie.txt" "../input"

    let bAreaMap@(nRows,nCols,areaMap) = parseInput contents
    print $ countTrees bAreaMap (3,1)
