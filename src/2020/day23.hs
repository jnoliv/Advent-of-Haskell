{-# Language BlockArguments #-}

import Advent.API (readInputDefaults)
import Data.Char (digitToInt, intToDigit)
import Data.List (elemIndex, notElem)
import Data.Maybe (fromJust)
import Control.Monad (unless, foldM_, zipWithM_, forM_)
import qualified Data.Vector.Unboxed.Mutable as VM

type CupCircle = VM.IOVector Int

encode :: Int -> [Int] -> IO CupCircle
encode size cups = do
    vector <- VM.new (succ size)

    let idx = [0 .. size]
    zipWithM_ (VM.write vector) idx $ map (generate size cups) idx

    return vector

generate :: Int -> [Int] -> Int -> Int
generate size cups i
    | i == 0    = -1
    | i <  n    = cups !! next i
    | i == size = 1
    | otherwise = i + 1
    where n      = length cups
          next i = succ . fromJust $ i `elemIndex` cups

play :: (Int, CupCircle, Int) -> IO (Int, CupCircle, Int)
play (idx, cups, size) = do
    let nextIdx x = if x > 1 then pred x else size

    cup1 <- VM.read cups idx
    cup2 <- VM.read cups cup1
    cup3 <- VM.read cups cup2
    next <- VM.read cups cup3

    let dest = until (`notElem` [cup1, cup2, cup3]) nextIdx (nextIdx idx)

    afterDest <- VM.read cups dest

    VM.write cups idx  next
    VM.write cups dest cup1
    VM.write cups cup3 afterDest

    return (next, cups, size)

playNMoves :: Int -> (Int, CupCircle, Int) -> IO ()
playNMoves n (idx, cups, size) = unless (n <= 0) do
    (idx', cups', _) <- play (idx, cups, size)
    playNMoves (pred n) (idx', cups', size)

order :: Int -> CupCircle -> Int -> IO [Int]
order 0 _ _ = return []
order n cups idx = do
    cup <- VM.read cups idx
    (cup :) <$> order (pred n) cups cup

-- |
-- >>> :main
-- 32897654
-- 186715244496
main :: IO()
main = do
    cups <- map digitToInt . init <$> readInputDefaults 2020 23

    let maxIdx = length cups

    cupsV <- encode maxIdx cups
    playNMoves 100 (head cups, cupsV, maxIdx)
    labels <- order (pred maxIdx) cupsV 1

    cupsV2 <- encode 1000000 cups
    playNMoves 10000000 (head cups, cupsV2, 1000000)
    labels2 <- order 2 cupsV2 1

    putStrLn . map intToDigit $ labels
    print . product $ labels2
