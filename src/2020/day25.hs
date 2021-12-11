import Advent.API (readInputDefaults)
import Data.Bifunctor (bimap)
import Data.List (iterate')

findLoopSize :: Int -> Int -> Int
findLoopSize sn key = fst $ until ((== key) . snd) (bimap succ transform) (0, 1)
    where transform x = (x * sn) `mod` 20201227

loop :: Int -> Int -> Int
loop n sn = iterate' ((`mod` 20201227) . (* sn)) 1 !! n

-- |
-- >>> :main
-- 9714832
main :: IO()
main = do
    [pubk1, pubk2] <- map read . lines <$> readInputDefaults 2020 25

    let loopSize1 = findLoopSize 7 pubk1
        encKey    = loop loopSize1 pubk2

    print encKey
