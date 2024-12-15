{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (readParsed, asciiChar, char, symbolChar, many, manyTill, sepBy, Parser, (<|>))
import Advent.Coord.Grid (down, left, right, up, Coord)
import Advent.Utils (readAsMap)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type Warehouse = Map Coord Char

parser :: Parser (Warehouse, String)
parser = (,) <$> warehouse <*> moves
    where
        warehouse = readAsMap (\c -> if c == '.' then Nothing else Just c) <$> manyTill asciiChar "\n\n"
        moves     = concat <$> many (char 'v' <|> symbolChar) `sepBy` "\n"

widen :: Warehouse -> Warehouse
widen = Map.fromList . concatMap (uncurry f) . Map.toList
    where
        f (y,x) '#' = [((y, 2*x), '#'), ((y, 2*x + 1), '#')]
        f (y,x) 'O' = [((y, 2*x), '['), ((y, 2*x + 1), ']')]
        f (y,x) '@' = [((y, 2*x), '@')]

getStart :: Warehouse -> (Warehouse, Coord)
getStart warehouse = (start `Map.delete` warehouse, start)
    where
        start = head . Map.keys $ Map.filter (== '@') warehouse

go :: Char -> Coord -> Coord
go '>' = right
go 'v' = down
go '<' = left
go '^' = up

move :: (Warehouse -> Char -> Coord -> Bool)
     -> (Warehouse -> Char -> Coord -> Warehouse)
     -> (Warehouse, Coord)
     -> Char
     -> (Warehouse, Coord)
move canPush push (warehouse, pos) dir
    | canMove next               = (warehouse , next)
    | canPush warehouse dir next = (warehouse', next)
    | otherwise                  = (warehouse , pos )
    where
        canMove = (`Map.notMember` warehouse)

        next       = go dir pos
        warehouse' = push warehouse dir next

canPushNarrow :: Warehouse -> Char -> Coord -> Bool
canPushNarrow warehouse dir pos = case pos `Map.lookup` warehouse of
    Nothing  -> True
    Just '#' -> False
    Just 'O' -> canPushNarrow warehouse dir (go dir pos)

pushNarrow :: Warehouse -> Char -> Coord -> Warehouse
pushNarrow warehouse dir pos = case pos `Map.lookup` warehouse of
    Nothing  -> warehouse
    Just 'O' -> (pos `Map.delete`) . Map.insert next 'O' $ pushNarrow warehouse dir next
        where
            next = go dir pos

canPushWide :: Warehouse -> Char -> Coord -> Bool
canPushWide warehouse dir pos = canPush' Set.empty dir pos
    where
        canPush' seen dir pos
            | pos `Set.member` seen = True
            | otherwise = 
                case pos `Map.lookup` warehouse of
                    Nothing  -> True
                    Just '#' -> False
                    Just '[' -> canPush' seen' dir (go dir pos) && canPush' seen' dir (go dir (right pos))
                    Just ']' -> canPush' seen' dir (go dir pos) && canPush' seen' dir (go dir (left  pos))
                where seen' = pos `Set.insert` seen

pushWide :: Warehouse -> Char -> Coord -> Warehouse
pushWide warehouse dir pos =
    case pos `Map.lookup` warehouse of
        Nothing  -> warehouse
        Just '[' -> moveBox pos '[' $ moveBox (right pos) ']' warehouse
        Just ']' -> moveBox pos ']' $ moveBox (left  pos) '[' warehouse
    where
        moveBox p c w = (p `Map.delete`) . Map.insert (go dir p) c $ pushWide w dir (go dir p)

gpsCoordinate :: Coord -> Int
gpsCoordinate (y,x) = 100 * y + x

-- |
-- >>> :main
-- 1383666
-- 1412866
main :: IO ()
main = do
    (warehouse, moves) <- readParsed 2024 15 parser

    let start1 = getStart        warehouse
        start2 = getStart (widen warehouse)

        (after1, _) = foldl (move canPushNarrow pushNarrow) start1 moves
        (after2, _) = foldl (move canPushWide   pushWide  ) start2 moves

    print . sum . map gpsCoordinate . Map.keys . Map.filter (== 'O') $ after1
    print . sum . map gpsCoordinate . Map.keys . Map.filter (== '[') $ after2
