module Utils (
    count, xor,
    sinsert, findSumPair,
    Parser, readParsedLines,
    readAsMap
    ) where

import AdventAPI (readInputDefaults)

import Data.Void (Void)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
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


---- Parsing abstractions

type Parser a = Parsec Void String a

-- | Read the input of the given day and apply the given
-- parser to all lines of said input
readParsedLines :: Int -> Parser a -> IO [a]
readParsedLines day parser = do
    input <- readInputDefaults day

    let parserFull = parser `endBy` char '\n' <* eof

    case parse parserFull "" input of
        Left e  -> fail $ "\n" ++ errorBundlePretty e
        Right r -> return r

-- | Read the ASCII input to a map. Receives a function to convert
-- from char to the map value, wrapped in Maybe to allow ommiting
-- positions from the resulting map
readAsMap :: (Char -> Maybe a) -> String -> Map.Map (Int, Int) a
readAsMap f input = Map.fromList . catMaybes . concatMap (map maybeT) $ zipWith zip inds ls
    where ls    = map (map f) . lines $ input
          inds  = map (`zip` [0..]) . fmap (cycle . return) $ [0..]
          maybeT (x, Just y)  = Just (x,y)
          maybeT (_, Nothing) = Nothing
