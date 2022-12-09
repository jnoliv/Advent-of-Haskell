{-# Language ImportQualifiedPost, BlockArguments #-}

module Advent.Utils (
    xor,
    findSumPair,
    md5,
    count, sinsert, replace, combinations,
    readBin, showBin,
    readAsMap, mapDimensions, showMap, readAsSet, showSet
) where

import Advent.Coord.Grid (Coord, (.+))
import Control.Monad (guard)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (digitToInt, intToDigit)
import Data.Function (on)
import Data.List (intercalate, tails)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Numeric (readInt, showIntAtBase, showHex)

-- | Boolean exclusive OR
xor :: Bool -> Bool -> Bool
xor = (/=)

-- | Finds a pair in the given list that sums to the given number.
-- Uses the two pointer technique to do so, which is probably not the
-- most efficient way to do this in Haskell. This assumes a sorted input
-- list! Check the algorithm here:
-- https://www.geeksforgeeks.org/two-pointers-technique/
findSumPair :: Integer -> [Integer] -> Maybe (Integer, Integer)
findSumPair sum list = findSumPair' sum 0 (length list - 1) list

findSumPair' :: Integer -> Int -> Int -> [Integer] -> Maybe (Integer, Integer)
findSumPair' _ _ _ [] = Nothing
findSumPair' sum l r list
    | l >= r = Nothing
    | curSum > sum  = findSumPair' sum l (r - 1) list
    | curSum < sum  = findSumPair' sum (l + 1) r list
    | curSum == sum = Just (left, right)
    where
        left  = list !! l
        right = list !! r
        curSum = left + right

md5 :: String -> String
md5 = toHex . MD5.hash . BSC.pack
    where
        toHex   = pad 32 . concatMap (pad 2 . flip showHex "") . BS.unpack
        pad l s = replicate (l - length s) '0' ++ s

---- List operations

-- | Count occurences in the given list that satisfy 'cond'
count :: (a -> Bool) -> [a] -> Int
count cond = length . filter cond

-- | Sorted list insertion
sinsert :: Ord a => a -> [a] -> [a]
sinsert x [] = [x]
sinsert x (y:ys)
    | x >  y = y : sinsert x ys 
    | x <= y = x : y : ys 

-- | Replace all occurrences of 'cur' by 'new' in the list
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace cur new (e:es)
    | e == cur  = new : replace cur new es
    | otherwise = e   : replace cur new es

-- | Return all combinations of size 'n' of the given list
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n l
    | n > length l = []
    | otherwise    = [x : c | (x : xs) <- tails l, c <- combinations (n - 1) xs]

---- Parsing and printing

-- | Read a binary number
readBin :: String -> Int
readBin = fst . head . readInt 2 (`elem` "01") digitToInt

-- | Show a number in binary
showBin :: Int -> String
showBin n = showIntAtBase 2 intToDigit n ""

-- | Read the ASCII input to a map. Receives a function to convert
-- from char to the map value, wrapped in Maybe to allow ommiting
-- positions from the resulting map.
readAsMap :: (Char -> Maybe a) -> String -> Map Coord a
readAsMap f input = Map.fromList do
    (r, line)  <- zip [0..] (lines input)
    (c, char)  <- zip [0..] line
    Just value <- [f char]
    return ((r,c), value)

-- | 
mapDimensions :: Map Coord a -> (Int,Int)
mapDimensions m = (x1 - x0 + 1, y1 - y0 + 1) 
    where
        ((y0,x0), (y1,x1)) = findDefininingPoints . map fst $ Map.toList m

-- | Show a rectangular map using 'f' to convert elements to
-- characters and 'def' as the default value in the map
showMap :: (a -> Char) -> a -> Map Coord a -> String
showMap f def m = intercalate "\n" $ map (map (f . findWithDefault)) indexes'
    where
        ((y0,x0), (y1,x1)) = findDefininingPoints . map fst $ Map.toList m
        nRows    = y1 - y0 + 1
        nCols    = x1 - x0 + 1
        indexes  = take nRows . map (take nCols) $ indexGrid2D
        indexes' = map (map ((y0,x0) .+)) indexes
        findWithDefault k = Map.findWithDefault def k m

indexGrid2D :: Integral a => [[(a,a)]]
indexGrid2D = map (`zip` [0..]) . fmap (cycle . return) $ [0..]

-- | Finds the top left and bottom right points that define the
-- smallest rectangle containing all the points in the list.
findDefininingPoints :: [Coord] -> (Coord, Coord)
findDefininingPoints (p:ps) = foldl f (p,p) ps
    where
        f ((y0,x0), (y1,x1)) (y,x) = ((min y0 y, min x0 x), (max y1 y, max x1 x))

-- | Read the ASCII input to a set. Receives a function to define
-- which characters should be included in the set (or their positions,
-- rather).
readAsSet :: (Char -> Bool) -> String -> Set Coord
readAsSet f input = Set.fromList do
    (r, line) <- zip [0..] (lines input)
    (c, char) <- zip [0..] line
    guard (f char)
    return (r,c)

-- | Show a rectangular set using 'f' to convert membership to characters
showSet :: (Bool -> Char) -> Set Coord -> String
showSet f s = intercalate "\n" [[f $ Set.member (r,c) s | c <- [c0 .. c1]] | r <- [r0 .. r1] ] ++ "\n"
    where
        ((r0,c0), (r1,c1)) = findDefininingPoints . Set.toList $ s
