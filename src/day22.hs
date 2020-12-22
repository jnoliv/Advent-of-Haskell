{-# Language OverloadedStrings #-}

import AdventAPI (readInputDefaults)
import Text.Megaparsec (endBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (Parser, parseWrapper)

type Game = ([Int], [Int])

format :: Parser Game
format = (,) <$> ("Player 1:\n"   *> decimal `endBy` "\n")
             <*> ("\nPlayer 2:\n" *> decimal `endBy` "\n")

turn :: Game -> Game
turn (c1:cs1, c2:cs2) = 
    if c1 > c2
        then (cs1 ++ [c1,c2], cs2)
        else (cs1, cs2 ++ [c2,c1])

play :: Game -> Game
play = until ((||) <$> null . fst <*> null . snd) turn

score :: Game -> Int
score = sum . zipWith (*) [1..] . reverse . winnerDeck
    where winnerDeck (d, []) = d
          winnerDeck ([], d) = d

main :: IO()
main = do
    gameStart <- parseWrapper format <$> readInputDefaults 22

    let end = play gameStart

    print $ score end
