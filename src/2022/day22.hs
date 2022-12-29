{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections #-}

import Advent.Megaparsec
import Advent.Coord.Grid
import Advent.Utils

import Data.Map (Map)
import Data.Map qualified as Map

--import Debug.Trace

data Tile = Void | Empty | Wall  -- | Fright | Fdown | Fleft | Fup
data Cmd  = Fd Int | Rt Char
    deriving Show
data Dir  = R | D | L | U
    deriving (Enum, Show)

format :: Parser (String, [Cmd])
format = (,) <$> manyTill asciiChar "\n\n" <*> some cmd
    where
        cmd = (Fd <$> decimal) <|> (Rt <$> letterChar)

readTile :: Char -> Maybe Tile
readTile ' ' = Nothing
readTile '.' = Just Empty
readTile '#' = Just Wall

showTile :: Tile -> Char
showTile Void  = ' '
showTile Empty = '.'
showTile Wall  = '#'

--showTile Fright = '>'
--showTile Fdown  = 'v'
--showTile Fleft  = '<'
--showTile Fup    = '^'
--
--toTile R = Fright
--toTile D = Fdown
--toTile L = Fleft
--toTile U = Fup

rotate :: Int -> Dir -> Dir
rotate n = toEnum . (`mod` 4) . (+ n) . fromEnum

move :: Dir -> Coord -> Coord
move R p = right p
move D p = down  p
move L p = left  p
move U p = up    p

followPath :: Map Coord Tile -> [Cmd] -> (Coord, Dir) -> (Coord, Dir)
followPath m []           result     = result
followPath m (cmd : cmds) (pos, dir) =
    case cmd of
        Rt 'L' -> followPath m cmds (pos, rotate (-1) dir)
        Rt 'R' -> followPath m cmds (pos, rotate   1  dir)
        Fd  n  -> followPath m cmds (forward m n dir pos, dir)

forward :: Map Coord Tile -> Int -> Dir -> Coord -> Coord
forward _ 0 _   pos = pos
forward m n dir pos =
    case next `Map.lookup` m of
        Just Wall  -> pos
        Just Empty -> forward m (n-1) dir next
        Nothing    -> case tile of
            Wall  -> pos 
            Empty -> forward m (n-1) dir next'
    where
        next = move dir pos

        (tile, next') = wrapAround m dir next

wrapAround :: Map Coord Tile -> Dir -> Coord -> (Tile, Coord)
wrapAround m R (y,x) = (\((_,x'),t) -> (t, (y, x'))) . Map.findMin $ Map.filterWithKey (\(y',_) _ -> y' == y) m
wrapAround m D (y,x) = (\((y',_),t) -> (t, (y', x))) . Map.findMin $ Map.filterWithKey (\(_,x') _ -> x' == x) m
wrapAround m L (y,x) = (\((_,x'),t) -> (t, (y, x'))) . Map.findMax $ Map.filterWithKey (\(y',_) _ -> y' == y) m
wrapAround m U (y,x) = (\((y',_),t) -> (t, (y', x))) . Map.findMax $ Map.filterWithKey (\(_,x') _ -> x' == x) m

toPassword :: (Coord, Dir) -> Int
toPassword ((y,x), d) = (y + 1) * 1000 + (x + 1) * 4 + f d
    where
        f R = 0
        f D = 1
        f L = 2
        f U = 3

-- |
-- >>> :main
-- 117102
main :: IO ()
main = do
    (rawBoard, path) <- readParsed 2022 22 format

    let board = readAsMap readTile rawBoard

        initial = fst . Map.findMin $ Map.filterWithKey (\(y,_) _ -> y == 0) board
        final   = followPath board path (initial, R)

    print $ toPassword final
