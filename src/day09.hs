import qualified Common.AdventAPI as AdventAPI

import Data.List (sort, delete)
import Common.Utils (sinsert, findSumPair)
import qualified Data.Vector as V

findFstInvalid :: V.Vector Integer -> Int -> Integer
findFstInvalid xmas wSize = findFstInvalid' xmas wSize (sort . V.toList . V.take wSize $ xmas) wSize

findFstInvalid' :: V.Vector Integer -> Int -> [Integer] -> Int -> Integer
findFstInvalid' xmas wSize window i =
    case findSumPair value window of
        Just _  -> findFstInvalid' xmas wSize window' (succ i)
        Nothing -> value
    where value   = xmas V.! i
          window' = sinsert value . delete (xmas V.! (i - wSize)) $ window

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 9

    let xmas    = map read . lines $ contents
        invalid = findFstInvalid (V.fromList xmas) 25
        

    print invalid
