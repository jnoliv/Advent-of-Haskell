import Advent.API (readInputDefaults)
import Advent.Utils (combinations)

findStart :: Int -> String -> Int -> Int
findStart n packet i = 
    if all areDiff combs
        then i + n
        else findStart n (tail packet) (i + 1)
    where
        chars   = take n packet
        combs   = combinations 2 chars

        areDiff [a,b] = a /= b

-- |
-- >>> :main
-- 1833
-- 3425
main :: IO ()
main = do
    packet <- readInputDefaults 2022 6

    let startOfPacket  = findStart  4 packet 0
        startOfMessage = findStart 14 packet 0

    print startOfPacket
    print startOfMessage
