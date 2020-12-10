import qualified Common.AdventAPI as AdventAPI

import Data.List (sort)
import Common.Utils (count)

joltDiff1x3 :: [Int] -> Int
joltDiff1x3 plugs = (*) <$> count (==1) <*> count (==3) $ diffs2
    where diffs  = zipWith (-) (tail plugs) plugs
          diffs2 = head plugs : diffs ++ [3]

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 10

    let plugs  = sort . map read . lines $ contents :: [Int]
    
    print $ joltDiff1x3 plugs
