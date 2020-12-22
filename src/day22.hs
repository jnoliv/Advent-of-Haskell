{-# Language OverloadedStrings #-}

import AdventAPI (readInputDefaults)
import Text.Megaparsec (endBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (Parser, parseWrapper)

data GameSt = Ongoing | Win Int
    deriving Eq

type Game = ([Int], [Int])
type RecGame = (Game, GameSt, [Game])

format :: Parser Game
format = (,) <$> ("Player 1:\n"   *> decimal `endBy` "\n")
             <*> ("\nPlayer 2:\n" *> decimal `endBy` "\n")

-----------------------------------------------------------------
turn :: Game -> Game
turn (c1:cs1, c2:cs2) = 
    if c1 > c2
        then (cs1 ++ [c1,c2], cs2)
        else (cs1, cs2 ++ [c2,c1])

play :: Game -> Game
play = until ((||) <$> null . fst <*> null . snd) turn

-----------------------------------------------------------------

playRec :: RecGame -> RecGame
playRec = until (\(_, st, _) -> st /= Ongoing) turnRec

turnRec :: RecGame -> RecGame
turnRec (g@(_,[]), _, prevGames) = (g, Win 1, prevGames)
turnRec (g@([],_), _, prevGames) = (g, Win 2, prevGames)
turnRec (g@(c1:cs1, c2:cs2), _, prevGames)
    | g `elem` prevGames = (g, Win 1, prevGames)
    | length cs1 >= c1 && length cs2 >= c2 =
        rearrange $ playRec ((take c1 cs1, take c2 cs2), Ongoing, [])
    | otherwise =
        rearrange (g, if c1 > c2 then Win 1 else Win 2, prevGames)
    where rearrange (_, Win 1, _) = ((cs1 ++ [c1,c2], cs2), Ongoing, g : prevGames)
          rearrange (_, Win 2, _) = ((cs1, cs2 ++ [c2,c1]), Ongoing, g : prevGames)

-----------------------------------------------------------------

score :: Game -> Int
score = sum . zipWith (*) [1..] . reverse . winnerDeck
    where winnerDeck (d, []) = d
          winnerDeck ([], d) = d

main :: IO()
main = do
    gameStart <- parseWrapper format <$> readInputDefaults 22

    print . score $ play gameStart
    print . score . (\(g,_,_) -> g) $ playRec (gameStart, Ongoing, [])
