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

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 15

    let startNums = map read . splitOn "," $ contents
        mem       = M.fromList . zip startNums $ zip [0..] [0..]
        n2020     = until ((==) 2020 . fst3) play (length startNums, last startNums, mem)
        n30000000 = until ((==) 30000000 . fst3) play (length startNums, last startNums, mem)

    print . snd3 $ n2020
    print . snd3 $ n30000000
