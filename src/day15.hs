import AdventAPI (readInputDefaults)
import qualified Data.IntMap as M
import Data.List.Split (splitOn)

type Mem = M.IntMap (Int,Int)   -- (last said, previous last said)

age :: Int -> Mem -> Int
age n = uncurry (-) . M.findWithDefault (0,0) n

say :: Int -> Int -> Mem -> Mem
say n turn mem = M.insert n turns mem
    where turns = (\(t1,_) -> (turn, t1)) $ M.findWithDefault (turn, turn) n mem

play :: (Int, Int, Mem) -> (Int, Int, Mem)
play (turn, last, mem) = (succ turn, a, mem')
    where a    = age last mem
          mem' = say a turn mem

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 15

    let startNums = map read . splitOn "," $ contents
        mem       = M.fromList . zip startNums $ zip [0..] [0..]
        n2020     = until (\(t,_,_) -> t == 2020) play (length startNums, last startNums, mem)

    print $ (\(_,n,_) -> n) n2020 
