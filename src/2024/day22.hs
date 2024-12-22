{-# LANGUAGE ImportQualifiedPost, BlockArguments #-}

import Advent.API (readInputDefaults)
import Data.Bits ((.&.), shiftR, shiftL, xor)
import Data.List (zip4)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

evolve :: Int -> Int
evolve n = n3
    where
        n1 = ((n  `shiftL`  6) `xor` n ) .&. 0xFFFFFF
        n2 = ((n1 `shiftR`  5) `xor` n1) .&. 0xFFFFFF
        n3 = ((n2 `shiftL` 11) `xor` n2) .&. 0xFFFFFF

mostBananas :: [Int] -> Int
mostBananas ns = maximum $ map countBananas seqs
    where
        seqsMaps = map seqsMap ns
        seqs     = Set.toList . Set.fromList $ concatMap Map.keys seqsMaps

        prices      = map (`mod` 10) . take 2001 . iterate evolve
        changes     = zipWith (-) <$> tail <*> id
        windows4 as = zip4 as (drop 1 as) (drop 2 as) (drop 3 as)

        seqsMap :: Int -> Map (Int,Int,Int,Int) Int
        seqsMap n = foldr (uncurry Map.insert) Map.empty kvs
            where
                ps  = prices n
                cs  = changes ps
                kvs = zip (windows4 cs) (drop 4 ps)

        countBananas seq = sum $ map (Map.findWithDefault 0 seq) seqsMaps

-- |
-- >>> :main
-- 19927218456
-- 2189
main :: IO ()
main = do
    input <- map (read :: String -> Int) . lines <$> readInputDefaults 2024 22

    print . sum $ map ((!! 2000) . iterate evolve) input
    print $ mostBananas input
