import qualified Common.AdventAPI as AdventAPI

import Control.Monad (forM_)
import qualified Data.Set as Set

type AreaMap = Set.Set (Int,Int)        -- set of all tree positions
type BoundedAreaMap = (Int,Int,AreaMap) -- (#rows, #cols, _)

-- | Prepend a value to a 2-tuple to form a 3-tuple
prependT :: a -> (b,c) -> (a,b,c)
prependT x (y,z) = (x,y,z)

-- | Remove the last element of a 3-tuple
truncateT :: (a,b,c) -> (a,b)
truncateT (a,b,_) = (a,b)

-- | Get the third element of a 3-tuple
thrd :: (a,b,c) -> c
thrd (_,_,c) = c

-- | Check if the given coordinates are a tree in the given area map
hasTree :: BoundedAreaMap -> (Int,Int) -> Bool
hasTree (_, nCols, areaMap) (x,y) = Set.member (x,newY) areaMap
    where newY = y `mod` nCols 

-- | Parse the input into a set of all positions with trees
parseInput :: String -> BoundedAreaMap
parseInput input = (length rawRows, length . head $ rawRows, areaMap)
    where rawRows = lines input
          rows    = map (zip [0..]) rawRows 
          all     = concat $ zipWith (map . prependT) [0..] rows
          trees   = map truncateT . filter ((=='#') . thrd) $ all
          areaMap = Set.fromList trees

-- | Count all the trees hit by following the given slope
countTrees :: BoundedAreaMap -> (Int, Int) -> Int
countTrees bAreaMap@(nRows,_,_) (hStep, vStep) =
    let positions = zip [0,vStep..nRows] [0,hStep..]
    in length . filter (hasTree bAreaMap) $ positions

main :: IO()
main = do
    contents <- AdventAPI.readInput 3 "../session-cookie.txt" "../input"

    let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

    let bAreaMap = parseInput contents        
        trees = map (countTrees bAreaMap) slopes
    
    print $ trees !! 1
    print $ product trees
