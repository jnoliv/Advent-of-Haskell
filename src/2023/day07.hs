{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (Parser, (<|>), some, decimal, readParsedLines)
import Advent.Utils (count)
import Data.List (sort, group, sortBy)
import Data.Function (on)
import Data.Functor.Classes (Ord1(liftCompare))

parser :: Parser (Hand, Int)
parser = (,) <$> (Hand <$> some card) <* " " <*> decimal
    where
        card = (Two    <$ "2")
           <|> (Three  <$ "3")
           <|> (Four   <$ "4")
           <|> (Five   <$ "5")
           <|> (Six    <$ "6")
           <|> (Seven  <$ "7")
           <|> (Eight  <$ "8")
           <|> (Nine   <$ "9")
           <|> (Ten    <$ "T")
           <|> (Jack   <$ "J")
           <|> (Queen  <$ "Q")
           <|> (King   <$ "K")
           <|> (Ace    <$ "A")

newtype Hand = Hand [Card]
    deriving (Eq, Ord)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Enum, Eq, Ord)

data Type = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Enum, Eq, Ord)

handType :: [Card] -> Type
handType hand = case (sort . map length . group . sort) hand of
    [5]       -> FiveOfAKind
    [1,4]     -> FourOfAKind
    [2,3]     -> FullHouse
    [1,1,3]   -> ThreeOfAKind
    [1,2,2]   -> TwoPair
    [1,1,1,2] -> OnePair
    _         -> HighCard

compareCardWild :: Card -> Card -> Ordering
compareCardWild Jack Jack = EQ
compareCardWild Jack _    = LT
compareCardWild _    Jack = GT
compareCardWild c1   c2   = c1 `compare` c2

handTypeWild :: [Card] -> Type
handTypeWild hand = case (count (== Jack) hand, handType hand) of
    (0,            t) -> t 
    (_,  FiveOfAKind) -> FiveOfAKind    -- JJJJJ
    (4,  FourOfAKind) -> FiveOfAKind    -- JJJJX
    (1,  FourOfAKind) -> FiveOfAKind    -- JXXXX
    (3,    FullHouse) -> FiveOfAKind    -- JJJXX
    (2,    FullHouse) -> FiveOfAKind    -- JJXXX
    (3, ThreeOfAKind) -> FourOfAKind    -- JJJXY
    (1, ThreeOfAKind) -> FourOfAKind    -- JXXXY
    (2,      TwoPair) -> FourOfAKind    -- JJXXY
    (1,      TwoPair) -> FullHouse      -- JXXYY
    (2,      OnePair) -> ThreeOfAKind   -- JJXYZ
    (1,      OnePair) -> ThreeOfAKind   -- JXXYZ
    (1,     HighCard) -> OnePair        -- JXYZW

compareHandWild :: Hand -> Hand -> Ordering
compareHandWild (Hand h1) (Hand h2)
        | typeOrd /= EQ = typeOrd
        | otherwise     = liftCompare compareCardWild h1 h2
        where 
            typeOrd = (compare `on` handTypeWild) h1 h2

-- |
-- >>> :main
-- 250120186
-- 250665248
main :: IO ()
main = do
    input <- readParsedLines 2023 7 parser

    let sorted   = sort input
        winnings = sum . zipWith (*) [1..] $ map snd sorted

        sortedWild   = sortBy (compareHandWild `on` fst) input
        winningsWild = sum . zipWith (*) [1..] $ map snd sortedWild

    print winnings
    print winningsWild
