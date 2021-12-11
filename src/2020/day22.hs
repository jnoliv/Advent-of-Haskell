{-# Language OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type Deck    = Seq.Seq Int
type Game    = (Deck, Deck)
data RecGame = Ongoing Game (Set.Set Game) | Win1 Deck | Win2 Deck

format :: Parser Game
format = (,) <$> (Seq.fromList <$> ("Player 1:\n"   *> decimal `endBy` "\n"))
             <*> (Seq.fromList <$> ("\nPlayer 2:\n" *> decimal `endBy` "\n"))

combat :: Game -> Deck
combat (Seq.Empty, d) = d
combat (d, Seq.Empty) = d
combat (c1 Seq.:<| d1, c2 Seq.:<| d2)
    | c1 > c2   = combat (append c1 c2 d1, d2)
    | otherwise = combat (d1, append c2 c1 d2)

recursiveCombat :: RecGame -> RecGame
recursiveCombat (Ongoing (Seq.Empty, d) _) = Win2 d
recursiveCombat (Ongoing (d, Seq.Empty) _) = Win1 d
recursiveCombat (Ongoing g@(c1 Seq.:<| d1, c2 Seq.:<| d2) rounds)
    | g `Set.member` rounds = Win1 d1
    | Seq.length d1 >= c1 && Seq.length d2 >= c2 =
        continue $ recursiveCombat (Ongoing (Seq.take c1 d1, Seq.take c2 d2) Set.empty)
    | otherwise = continue $ if c1 > c2 then Win1 Seq.Empty else Win2 Seq.Empty
    where
        rearrange (Win1 _) = (append c1 c2 d1, d2)
        rearrange (Win2 _) = (d1, append c2 c1 d2)
        continue rg = recursiveCombat (Ongoing (rearrange rg) (Set.insert g rounds))

append :: Int -> Int -> Deck -> Deck
append c1 c2 d = d Seq.|> c1 Seq.|> c2

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList

recursiveScore :: RecGame -> Int
recursiveScore (Win1 d) = score d
recursiveScore (Win2 d) = score d

-- |
-- >>> :main
-- 34324
-- 33259
main :: IO()
main = do
    gameStart <- parseWrapper format <$> readInputDefaults 2020 22

    print . score . combat $ gameStart
    print . recursiveScore . recursiveCombat $ Ongoing gameStart Set.empty 
