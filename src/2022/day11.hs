{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (Parser, readParsed, ($>), try, (<|>), sepBy1, trim, decimal)
import Advent.Utils (sortDesc)
import Control.Monad (join)
import Data.List (sortBy)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

type Op      = Int -> Int
type Monkeys = Map Int Monkey

data Test   = Test Int Int Int
data Monkey = Monkey {
    mId         :: Int,
    items       :: [Int],
    op          :: Op,
    test        :: Test,
    inspections :: Int
}

format :: Parser [Monkey]
format = (Monkey <$> id <*> items <*> op <*> test <*> pure 0) `sepBy1` "\n"
    where
        decimals = decimal `sepBy1` ", "

        id    =          trim "Monkey "                        decimal ":\n"
        items =          trim "  Starting items: "             decimals "\n"
        test  = Test <$> trim "  Test: divisible by "          decimal  "\n"
                     <*> trim "    If true: throw to monkey "  decimal  "\n"
                     <*> trim "    If false: throw to monkey " decimal  "\n"
        op    =          trim "  Operation: new = old " op' "\n"

        op' :: Parser Op
        op'   = try ("* old" $> join (*))
            <|> try ("+ old" $> join (+))
            <|>     ((*) <$> ("* " *> decimal))
            <|>     ((+) <$> ("+ " *> decimal))

playRound :: Int -> Op -> Monkeys -> Monkeys
playRound n divisor monkeys = foldl (takeTurn divisor) monkeys [0..n-1]

takeTurn :: Op -> Monkeys -> Int -> Monkeys
takeTurn divisor monkeys id = Map.insert id monkey' monkeys'
    where 
        monkey@(Monkey {items = mItems, inspections = mInspections}) = fromJust $ Map.lookup id monkeys

        monkey'  = monkey {items = [], inspections = mInspections + length mItems}
        monkeys' = foldl (inspect divisor monkey) monkeys mItems

inspect :: Op -> Monkey -> Monkeys -> Int -> Monkeys
inspect divisor (Monkey {op = op', test = test'}) monkeys item =
    throw newWorry newMonkeyId monkeys
    where
        newWorry    = divisor (op' item)
        newMonkeyId = throwTo test' newWorry

throwTo :: Test -> Int -> Int
throwTo (Test div id1 id2) worry
    | worry `mod` div == 0 = id1
    | otherwise            = id2

throw :: Int -> Int -> Monkeys -> Monkeys
throw item to monkeys = Map.insert to (monkey {items = newItems}) monkeys
    where
        monkey@(Monkey {items = items'}) = fromJust $ Map.lookup to monkeys
        newItems                         = items' ++ [item]

monkeyBusiness :: Monkeys -> Int
monkeyBusiness = product . take 2 . sortDesc . map (inspections . snd) . Map.toList

-- |
-- >>> :main
-- 51075
-- 11741456163
main :: IO ()
main = do
    monkeys <- readParsed 2022 11 format

    let monkeysMap = Map.fromList $ zip [0..] monkeys
        nMonkeys   = length monkeys

        prodDivs = product $ map ((\(Test d _ _) -> d) . test) monkeys

        rounds  = iterate (playRound nMonkeys (`div` 3))        monkeysMap
        rounds2 = iterate (playRound nMonkeys (`mod` prodDivs)) monkeysMap

    print . monkeyBusiness $ rounds  !! 20
    print . monkeyBusiness $ rounds2 !! 10000
