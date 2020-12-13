import AdventAPI (readInputDefaults)

import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

-- | Find the smallest multiple of 'n' that is bigger than 'limit'
waitTime :: Integral a => a -> a-> a
waitTime limit n = n - (limit `mod` n)

-- | Find the earliest departure bus and time after 't'
findEarliestDeparture :: Integral a => a -> [a] -> (a, a)
findEarliestDeparture t = minimum . map ((,) <$> waitTime t <*> id)

-- | Prepare to input to 'chineseRemainder'
toChinRemInput :: [(Integer,Integer)] -> [(Integer,Integer)]
toChinRemInput = map (\(n,a) -> (waitTime a n, n))

parseInput :: String -> (Integer, [(Integer,Integer)])
parseInput input = (t, ids)
    where ls = lines input
          t   = read . head $ ls
          ids = map ((,) <$> read . fst <*> snd) . filter ((/=) "x" . fst) . (`zip` [0..]) . splitOn "," . (!! 1) $ ls

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 13

    let (t, ids) = parseInput contents
        depart   = findEarliestDeparture t $ map fst ids

    print . uncurry (*) $ depart
    print . fromJust . chineseRemainder . toChinRemInput $ ids
