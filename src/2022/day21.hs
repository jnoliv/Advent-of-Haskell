{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

data Op = N Int | Op String (Int -> Int -> Int) String

format :: Parser (String, Op)
format = (,) <$> some letterChar <* ": "
             <*> op
    where
        op    = (N  <$> decimal)
            <|> (Op <$> some letterChar <*> trim " " binop " "<*> some letterChar)
        binop = ((+) <$ "+")
            <|> ((-) <$ "-")
            <|> ((*) <$ "*")
            <|> (div <$ "/")
        
yell :: Map String Op -> String -> Map String Int -> (Int, Map String Int)
yell monkeys monkey yelled =
    case monkey `Map.lookup` monkeys of
        Just (N n)         -> (n,      Map.insert monkey n      yelled)
        Just (Op m1 op m2) -> (toYell, Map.insert monkey toYell yelled'')
            where
                (n1, yelled')  = getYelled m1 yelled
                (n2, yelled'') = getYelled m2 yelled'

                toYell = n1 `op` n2
                
                getYelled monkey yelled =
                    case monkey `Map.lookup` yelled of
                        Just n    -> (n, yelled)
                        otherwise -> yell monkeys monkey yelled

binarySearch :: (Int -> Ordering) -> Int -> Int -> Int
binarySearch p low high =
    let mid = (low + high) `div` 2 in
    case p mid of
        LT -> binarySearch p low mid
        GT -> binarySearch p mid high
        EQ -> mid        

-- |
-- >>> :main
-- 276156919469632
-- 3441198826073
main :: IO ()
main = do
    monkeys <- Map.fromList <$> readParsedLines 2022 21 format

    let rootYell = fst $ yell monkeys "root" Map.empty

        (Op m1 _ m2) = fromJust $ "root" `Map.lookup` monkeys
        (y1, yells1) = yell monkeys m1 Map.empty
        (y2, yells2) = yell monkeys m2 Map.empty

        pred' y n   = flip compare y . fst $ yell (Map.insert "humn" (N n) monkeys) m1 Map.empty

        pred = if "humn" `Map.member` yells1
            then pred' y2
            else pred' y1

        humnYell = binarySearch (pred' y2) 0 99999999999999999

    print rootYell
    print humnYell
