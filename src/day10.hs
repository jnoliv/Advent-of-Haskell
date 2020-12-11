import AdventAPI
import Data.List (sort)
import Data.List.Split (splitOn)
import Utils (count)

-- | Create a list of the differences between each element in plugs,
-- ex: [1,2,4,7] -> [1,1,2,3,3]. The first and last elements are the
-- differences to 0 and the maximum plus 3. Return the product of the
-- count of 1s and 3s in this list, as well as the differences list.
joltDiff1x3 :: [Int] -> (Int, [Int])
joltDiff1x3 plugs = (prod1x3, diffs)
    where diffs   = (++ [3]) . zipWith (-) plugs $ 0 : plugs
          prod1x3 = (*) <$> count (==1) <*> count (==3) $ diffs

-- | This one was fun. So I noticed the differences list of all
-- examples and input contained only 1s and 3s. Also, the maximum
-- number of consecutive 1s is 4.
-- The idea here is that each 3 on that list forces two numbers to
-- be in all arrangements. So we split the diff list on [3] and
-- filter out the lists where there aren't at least two 1s. Then we
-- calculate the length minus one of all the remaining lists, as one
-- of the 1s is fixed (the one before a 3). Then just map 2^ to all
-- and get the product. With a caveat, if there are three 1s in a row,
-- we must remove the case where none of the three numbers is selected.
numArrangements :: [Int] -> Int
numArrangements = product . map (nArrange . pred . length) . filter ((> 1) . length) . splitOn [3]
    where nArrange l = if l == 3 then pred $ 2 ^ l else 2 ^l

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 10

    let plugs            = sort . map read . lines $ contents :: [Int]
        (prod1x3, diffs) = joltDiff1x3 plugs

    print prod1x3
    print . numArrangements $ diffs
