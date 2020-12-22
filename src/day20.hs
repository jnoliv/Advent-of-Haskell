{-# Language OverloadedStrings #-}

import AdventAPI (readInputDefaults)
import Data.Bifunctor (second)
import Data.List (intersect)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Utils (readAsMap, binToDec, readBin, showBin, count)

type ImageData = M.Map (Int,Int) Int

data Tile = Tile { tileId :: Integer, image :: ImageData, neighbours :: [Integer] }
    deriving Show

-- Border operations

border :: ((Int,Int) -> Int -> Bool) -> ImageData -> Int
border f = binToDec . map snd . M.toAscList . M.filterWithKey f

topBorder, bottomBorder, leftBorder, rightBorder :: Int -> ImageData -> Int
topBorder    _    = border (\k _ -> fst k == 0) 
bottomBorder size = border (\k _ -> fst k == size - 1)
leftBorder   size = border (\k _ -> snd k == size - 1)
rightBorder  _    = border (\k _ -> snd k == 0)

invert :: Int -> Int -> Int
invert n b = readBin . reverse $ binPadded
    where bin       = showBin b
          binPadded = replicate (n - length bin) '0' ++ bin

normalize :: Int -> Int -> Int
normalize n b = min b $ invert n b

borders :: Int -> ImageData -> [Int]
borders n img = map (($ img) . ($ n)) [topBorder, rightBorder, bottomBorder, leftBorder]

-- Match tiles

matchNeighbours :: Int -> [(Integer, ImageData)] -> [Tile]
matchNeighbours n tiles = map (\(tid, img) -> Tile tid img (findNeighbours tid withBorders)) tiles
    where withBorders = map (second (map (normalize n) . borders n)) tiles

findNeighbours :: Integer -> [(Integer, [Int])] -> [Integer]
findNeighbours tid withBorders = map fst . filter ((== 1) . length . intersect ownBorders . snd) $ otherTiles 
    where otherTiles = filter ((/= tid) . fst) withBorders
          ownBorders = snd . head . filter ((== tid) . fst) $ withBorders

-- Parse

parseInput :: String -> [(Integer, ImageData)]
parseInput = map parseTile . splitOn "\n\n"

parseTile :: String -> (Integer, ImageData)
parseTile tile = (tid, tileMap)
    where tid     = read . take 4 . drop 5 $ tile
          tileMap = readAsMap (\c -> if c == '#' then Just 1 else Just 0) $ drop 11 tile

main :: IO()
main = do
    input <- readInputDefaults 20

    let rawTiles  = parseInput input
        tileSize  = round . sqrt . fromIntegral . M.size . snd . head $ rawTiles

        tiles     = matchNeighbours tileSize rawTiles
        corners   = filter ((== 2) . length . neighbours) $ tiles

    print . product . map tileId $ corners
