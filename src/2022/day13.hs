{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (Parser, readParsed, parseWrapper, sepBy, sepBy1, trim, (<|>), decimal)

import Data.List (sortBy, elemIndex)
import Data.Maybe (isJust, fromJust)

data Value = VInt Integer | VList [Value]
    deriving Eq

format :: Parser [(Value, Value)]
format = pairs `sepBy1` "\n"
    where
        pairs  = (,) <$> formatList <* "\n" <*> formatList <* "\n"

formatList :: Parser Value
formatList = VList <$> trim "[" (value `sepBy` ",") "]"
    where 
        value = VInt <$> decimal
            <|> formatList

inOrder :: (Value, Value) -> Maybe Bool
inOrder (VList [], VList []) = Nothing
inOrder (VList [], VList _)  = Just True
inOrder (VList _ , VList []) = Just False

inOrder (VList l, VList r) =
    case inOrder (head l, head r) of
        Just b  -> Just b
        Nothing -> inOrder (VList (tail l), VList (tail r))

inOrder (VInt a, VInt b)
    | a < b     = Just True
    | a > b     = Just False
    | otherwise = Nothing

inOrder (VInt l , VList r) = inOrder (VList [VInt l], VList r)
inOrder (VList l, VInt r)  = inOrder (VList l       , VList [VInt r]) 

toOrdering :: Maybe Bool -> Ordering
toOrdering (Just True)  = LT
toOrdering Nothing      = EQ
toOrdering (Just False) = GT

-- |
-- >>> :main
-- 5013
-- 25038
main :: IO ()
main = do
    -- PART 1
    packetPairs <- readParsed 2022 13 format

    let evaledPairs  = map inOrder packetPairs
        indexed      = filter (\(i, v) -> if isJust v then fromJust v else False) $ zip [1..] evaledPairs

    print . sum $ map fst indexed

    -- PART 2
    rawPackets <- filter (/= "") . lines <$> readInputDefaults 2022 13

    let packets       = map (parseWrapper formatList) rawPackets

        divider1      = VList [VList [VInt 2]]
        divider2      = VList [VList [VInt 6]]

        withDividers  = packets ++ [divider1, divider2]

        ordered       = sortBy (\a b -> toOrdering $ inOrder (a,b)) withDividers

        indexDivider1 = (+1) . fromJust $ divider1 `elemIndex` ordered
        indexDivider2 = (+1) . fromJust $ divider2 `elemIndex` ordered

    print $ indexDivider1 * indexDivider2
