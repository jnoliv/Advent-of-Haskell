{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (Parser, readParsed, decimal)
import Advent.Utils (combinations)
import Data.Array(listArray, (!))
import Data.Map (Map)
import Data.Map qualified as Map

import Debug.Trace

format :: Parser (Int,Int)
format = (,) <$> player <*> player
    where
        player = "Player " *> decimal *> " starting position: " *> decimal <* "\n"

movesP1 = [6,4,2,0,8]
movesP2 = [5,3,1,9,7]

-- |
-- >>> :main
-- 678468
main :: IO ()
main = do
    (p1, p2) <- readParsed 2021 21 format

    let (n, (_,s1), (_,s2), _, r) = play p1 p2

        res = if n == 1 then s1 * r else s2 * r

    print res
    print . uncurry max $ wins p1 p2


play :: Int -> Int -> (Int, (Int,Int), (Int,Int), Int, Int)
play p1 p2 = until f turn (1, (p1,0), (p2,0), 1, 0)
    where
        f :: (Int, (Int,Int), (Int,Int), Int, Int) -> Bool
        f (_, (_,s1), (_,s2), _, _) = s1 >= 1000 || s2 >= 1000

turn :: (Int, (Int,Int), (Int,Int), Int, Int) -> (Int, (Int,Int), (Int,Int), Int, Int)
turn (n, t1, t2, d, r)
    | n == 1    = (2, step t1, t2, d', r + 3)
    | otherwise = (1, t1, step t2, d', r + 3)
    where
        m  = (3 * d + 3) `mod` 10
        d' = d + 3 `mod` 10

        step (p,s) = let p' = move (p + m) in (p', s + p')

move :: Int -> Int
move v
    | v <= 10   = v
    | otherwise = v - 10


wins :: (Num a, Num b) => Int -> Int -> (a, b)
wins p1 p2 = memo ! (1,p1,0,p2,0)
    where
        (maxP, maxS)  = (10,30)
        possibleRolls = [x + y + z | x <- [1,2,3], y <- [1,2,3], z <- [1,2,3]]

        memo = listArray ((1,1,0,1,0), (2,maxP,maxS,maxP,maxS))
            [f t (p1,s1) (p2,s2) | t <- [1,2], p1 <- [1..maxP], s1 <- [0..maxS], p2 <- [1..maxP], s2 <- [0..maxS]]

        f t t1@(_,s1) t2@(_,s2)
            | s1 >= 21  = (1,0)
            | s2 >= 21  = (0,1)
            | t == 1    = foldl1 (.+) $ map (\d -> memo ! index 2 (step t1 d) t2) possibleRolls
            | otherwise = foldl1 (.+) $ map (\d -> memo ! index 1 t1 (step t2 d)) possibleRolls
            where
                (w1,l1) .+ (w2,l2) = (w1 + w2, l1 + l2)

                step (p,s) d            = let p' = move (p + d) in (p', s + p')
                index t (p1,s1) (p2,s2) = (t,p1,s1,p2,s2)
