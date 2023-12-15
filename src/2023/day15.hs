{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt, ord)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map

type KV = (String,Int)
data Op = Add String Int | Del String

toOp :: String -> Op
toOp str
    | last str == '-' = Del (init str)
    | otherwise       = Add (init . init $ str) (digitToInt $ last str)

hash :: String -> Int
hash = foldl f 0
    where
        f acc c = ((acc + ord c) * 17) `rem` 256

hashmap ::Map Int [KV] -> Op -> Map Int [KV]
hashmap hm (Del key)     = modifyBox hm  delete                key
hashmap hm (Add key val) = modifyBox hm (prependOrReplace val) key

modifyBox :: Map Int [KV] -> (String -> [KV] -> [KV]) -> String -> Map Int [KV]
modifyBox hm f key = flip (Map.insert hashedKey) hm . f key $ Map.findWithDefault [] hashedKey hm
    where
        hashedKey = hash key

delete :: String -> [KV] -> [KV]
delete key = filter ((/= key) . fst)

prependOrReplace :: Int -> String -> [KV] -> [KV]
prependOrReplace val key            [] = [(key,val)]
prependOrReplace val key ((k,v) : kvs)
    | k == key  = (key,val) : kvs
    | otherwise = (  k,  v) : prependOrReplace val key kvs


power :: Map Int [KV] -> Int
power = sum . map (uncurry (*) . bimap (+1) powerBox) . Map.assocs
    where
        powerBox = sum . zipWith (*) [1..] . map snd 

-- |
-- >>> :main
-- 514394
-- 236358
main :: IO ()
main = do
    steps <- splitOn "," . init <$> readInputDefaults 2023 15

    let ops = map toOp steps

    print . sum   $ map hash steps
    print . power $ foldl hashmap Map.empty ops