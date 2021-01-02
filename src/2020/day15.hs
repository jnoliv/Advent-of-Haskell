import AdventAPI (readInputDefaults)
import qualified Data.IntMap as M
import Data.List.Split (splitOn)

type Mem  = M.IntMap (Int,Int)   -- (last said, previous last said)
data Game = Game { turn :: !Int, prev :: !Int, mem :: !Mem}

age :: Int -> Mem -> Int
age n = uncurry (-) . M.findWithDefault (0,0) n

say :: Int -> Int -> Mem -> Mem
say n turn mem = M.insert n turns mem
    where turns = (\(t1,_) -> (turn, t1)) $ M.findWithDefault (turn, turn) n mem

play :: Game -> Game
play (Game turn last mem) = Game (succ turn) a mem'
    where a    = age last mem
          mem' = say a turn mem

-- |
-- >>> :main
-- 1618
-- 548531
main :: IO()
main = do
    contents <- readInputDefaults 2020 15

    let startNums = map read . splitOn "," $ contents
        mem       = M.fromList . zip startNums $ zip [0..] [0..]
        n2020     = until ((==)     2020 . turn) play $ Game (length startNums) (last startNums) mem
        n30000000 = until ((==) 30000000 . turn) play $ Game (length startNums) (last startNums) mem

    print . prev $ n2020
    print . prev $ n30000000
