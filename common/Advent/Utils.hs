module Advent.Utils (
    count, xor,
    sinsert, replace, findSumPair,
    binToDec, readBin, showBin,
    Parser, readParsedLines, parseLines, parseWrapper,
    readAsMap, showMap, readAsSet
) where

import AdventAPI (readInputDefaults)
import Data.Char (digitToInt, intToDigit)
import Data.Function (on)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Numeric (readInt, showIntAtBase)
import Text.Megaparsec (Parsec, parse, eof, endBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty)

-- | Count occurences in the given list that satisfy 'cond'
count :: (a -> Bool) -> [a] -> Int
count cond = length . filter cond

-- | Boolean exclusive OR
xor :: Bool -> Bool -> Bool
xor = (/=)

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


---- Parsing and printing

-- | Convert a list of bits to a decimal number
binToDec :: Integral a => [a] -> a
binToDec = foldl (\acc bit -> 2*acc + bit) 0

-- | Read a binary number
readBin :: String -> Int
readBin = fst . head . readInt 2 (`elem` "01") digitToInt

-- | Show a number in binary
showBin :: Int -> String
showBin n = showIntAtBase 2 intToDigit n ""

type Parser a = Parsec Void String a

-- | Read the input of the given day and apply the given
-- parser to all lines of said input
readParsedLines :: Int -> Parser a -> IO [a]
readParsedLines day parser = do
    input <- readInputDefaults day
    return $ parseLines parser input

-- | Apply the given parser to all lines of said input
parseLines :: Parser a -> String -> [a]
parseLines parser input =
    let parserFull = parser `endBy` char '\n' <* eof
    in parseWrapper parserFull input

-- | Apply the given parser to the given input. Manages
-- error reporting.
parseWrapper :: Parser a -> String -> a
parseWrapper parser input =
    case parse parser "" input of
        Left e  -> error $ "\n" ++ errorBundlePretty e
        Right r -> r

indexGrid2D :: Integral a => [[(a,a)]]
indexGrid2D = map (`zip` [0..]) . fmap (cycle . return) $ [0..]

-- | Read the ASCII input to a map. Receives a function to convert
-- from char to the map value, wrapped in Maybe to allow ommiting
-- positions from the resulting map.
readAsMap :: (Char -> Maybe a) -> String -> Map.Map (Int, Int) a
readAsMap f input = Map.fromList . catMaybes . concatMap (map maybeT) $ zipWith zip indexGrid2D ls
    where ls = map (map f) . lines $ input
          maybeT (x, Just y)  = Just (x,y)
          maybeT (_, Nothing) = Nothing

-- | Show a rectangular map using 'f' to convert elements to
-- characters and 'def' as the default value in the map
showMap :: (a -> Char) -> a -> Map.Map (Int, Int) a -> String
showMap f def m = intercalate "\n" $ map (map (f . findWithDefault)) indexes
    where (y0,x0) = fst $ Map.findMin m
          (y1,x1) = fst $ Map.findMax m
          nRows   = y1 - y0 + 1
          nCols   = x1 - x0 + 1
          indexes = take nRows . map (take nCols) $ indexGrid2D
          findWithDefault k = Map.findWithDefault def k m

-- | Read the ASCII input to a set. Receives a function to define
-- which characters should be included in the set (or their positions,
-- rather).
readAsSet :: (Char -> Bool) -> String -> Set.Set (Int,Int)
readAsSet f input = Set.fromList . map fst . filter snd . concat $ zipWith zip indexGrid2D ls
    where ls = map (map f) . lines $ input
