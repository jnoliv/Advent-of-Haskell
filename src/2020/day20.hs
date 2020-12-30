{-# Language OverloadedStrings #-}

import AdventAPI (readInputDefaults)
import Advent.Utils (readAsMap, binToDec, readBin, showBin, count)
import Data.Bifunctor (first, second)
import Data.Function (on)
import Data.List (intersect, sortOn, notElem)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe)

type ImageData = M.Map (Int,Int) Int

data Tile = Tile { tileId :: Integer, image :: ImageData, neighbours :: [Integer] }

instance Eq Tile where
  (==) = (==) `on` tileId

type TileMap = M.Map (Int,Int) Tile

-- Border operations

border :: ((Int,Int) -> Int -> Bool) -> ImageData -> Int
border f = binToDec . map snd . M.toAscList . M.filterWithKey f

topBorder, bottomBorder, leftBorder, rightBorder :: Int -> ImageData -> Int
topBorder    _    = border (\k _ -> fst k == 0) 
bottomBorder size = border (\k _ -> fst k == size - 1)
leftBorder   _    = border (\k _ -> snd k == 0)
rightBorder  size = border (\k _ -> snd k == size - 1)

invert :: Int -> Int -> Int
invert n b = readBin . reverse $ binPadded
    where bin       = showBin b
          binPadded = replicate (n - length bin) '0' ++ bin

normalize :: Int -> Int -> Int
normalize n b = min b $ invert n b

borders :: Int -> ImageData -> [Int]
borders n img = map (($ img) . ($ n)) [topBorder, rightBorder, bottomBorder, leftBorder]

bordersWithCoords :: Int -> ImageData -> [(Int, (Int,Int))]
bordersWithCoords n img = map (first (($ img) . ($ n)))
    [(topBorder, (-1,0)), (rightBorder, (0,1)), (bottomBorder, (1,0)), (leftBorder, (0,-1))]

-- Tile transformations

flipX :: Int -> ImageData -> ImageData
flipX n = M.mapKeys (\(y,x) -> (y, n - x - 1))

flipY :: Int -> ImageData -> ImageData
flipY n = M.mapKeys (\(y,x) -> (n - y - 1, x))

rotateR :: Int -> ImageData -> ImageData
rotateR n = M.mapKeys (\(y,x) -> (x, n - y - 1))

transformations :: Int -> ImageData -> [ImageData]
transformations n img = map ($ img)
    [id,
     rotateR n,
     rotateR n . rotateR n,
     rotateR n . rotateR n . rotateR n,
     flipX n,
     flipY n,
     flipX n . rotateR n,
     flipY n . rotateR n]

-- Match tiles

matchNeighbours :: Int -> [(Integer, ImageData)] -> [Tile]
matchNeighbours n tiles = map (\(tid, img) -> Tile tid img (findNeighbours tid withBorders)) tiles
    where withBorders = map (second (map (normalize n) . borders n)) tiles

findNeighbours :: Integer -> [(Integer, [Int])] -> [Integer]
findNeighbours tid withBorders = map fst . filter ((== 1) . length . intersect ownBorders . snd) $ otherTiles 
    where otherTiles = filter ((/= tid) . fst) withBorders
          ownBorders = snd . head . filter ((== tid) . fst) $ withBorders

-- Assemble image

-- | Return a mapping from tile ID to coordinates relative to pinned corner piece.
-- This in an abomination and I'm not proud of it. But I can't take anymore, I just want it over with.
assemble :: Int -> [Tile] -> TileMap -> ImageData
assemble n tiles decided
    | length decided == length tiles = removeBorders n . normalizeCoords $ decided
    | otherwise                      = assemble n tiles decided'
    where
        decided'                    = fst $ M.mapAccumWithKey foldF decided decided
        foldF acc k v               = (addNeighbours acc k v, v)
        addNeighbours map' pos tile = M.union map' . M.fromList . mapMaybe (findNeighbour map' pos tile) $ bordersWithCoords n (image tile)
        addCoord (y0,x0) (y1,x1)    = (y0 + y1, x0 + x1)
        getTile tid                 = head $ filter ((== tid) . tileId) tiles

        findNeighbour map' pos tile (bord, delta) =
            let pos' = addCoord pos delta
            in if pos' `M.member` map' then Nothing else matchNeighbour tile bord pos' delta
        
        matchNeighbour tile bord destP ( 0,  1) = matchNeighbour' tile bord destP (  leftBorder n)
        matchNeighbour tile bord destP ( 0, -1) = matchNeighbour' tile bord destP ( rightBorder n)
        matchNeighbour tile bord destP ( 1,  0) = matchNeighbour' tile bord destP (   topBorder n)
        matchNeighbour tile bord destP (-1,  0) = matchNeighbour' tile bord destP (bottomBorder n)
        
        matchNeighbour' tile bord destP getBord = if null matchingNeighs then Nothing else Just (destP, head matchingNeighs)
            where neighs                     = filter (`notElem` M.elems decided) . map getTile $ neighbours tile
                  getMatchingTransform neigh = listToMaybe . map (\i -> neigh {image = i}) . filter ((== bord) . getBord) . transformations n $ image neigh
                  matchingNeighs             = mapMaybe getMatchingTransform neighs

normalizeCoords :: M.Map (Int,Int) a -> M.Map (Int,Int) a
normalizeCoords mapping = M.mapKeys (\(y,x) -> (y - y0, x - x0)) mapping
    where (y0,x0) = fst $ M.findMin mapping

removeBorders :: Int -> TileMap -> ImageData
removeBorders n tiles = M.unions . M.elems . M.mapWithKey translate $ withoutBorders
    where withoutBorders    = M.map (normalizeCoords . M.filterWithKey notBorder . image) tiles
          notBorder (y,x) _ = y /= 0 && y /= n - 1 && x /= 0 && x /= n - 1
          n'                = n - 2
          translate (y,x)   = M.mapKeys (\(y',x') -> (y * n' + y', x * n' + x'))

-- Find sea monsters

findTransform :: ImageData -> ImageData
findTransform img = head . filter hasMonster $ transformations n img
    where n = (fst . fst $ M.findMax img) - (fst . fst $ M.findMin img) + 1

-- As seen from the first '#' on the left
--                   # 
-- #    ##    ##    ###
--  #  #  #  #  #  #   
monsterCoords :: [(Int,Int)]
monsterCoords = [(0,0), (1,1), (1,4), (0,5), (0,6), (1,7), (1,10), (0,11), (0,12), (1,13), (1,16), (0,17), (-1,18), (0,18), (0,19)]

hasMonster :: ImageData -> Bool
hasMonster img = or . M.elems . M.mapWithKey (\k _ -> hasMonsterHere k img) $ img

hasMonsterHere :: (Int,Int) -> ImageData -> Bool
hasMonsterHere (y,x) img = all (isHash . addCoord) monsterCoords
    where addCoord (y1,x1) = (y + y1, x + x1)
          isHash pos       = (M.findWithDefault 0 pos img) == 1

removeMonsters :: ImageData -> ImageData
removeMonsters img = M.foldrWithKey foldF img img
    where foldF k _ acc = if hasMonsterHere k acc then removeMonster k acc else acc

removeMonster :: (Int,Int) -> ImageData -> ImageData
removeMonster (y,x) img = foldl (\m pos -> M.delete (addCoord pos) m) img monsterCoords
    where addCoord (y1,x1) = (y + y1, x + x1)

-- Parse

parseInput :: String -> [(Integer, ImageData)]
parseInput = map parseTile . splitOn "\n\n"

parseTile :: String -> (Integer, ImageData)
parseTile tile = (tid, tileMap)
    where tid     = read . take 4 . drop 5 $ tile
          tileMap = readAsMap (\c -> if c == '#' then Just 1 else Just 0) $ drop 11 tile

main :: IO()
main = do
    input <- readInputDefaults 2020 20

    let rawTiles  = parseInput input
        tileSize  = round . sqrt . fromIntegral . M.size . snd . head $ rawTiles

        tiles     = matchNeighbours tileSize rawTiles
        corners   = filter ((== 2) . length . neighbours) tiles

        img       = assemble tileSize tiles . M.singleton (0,0) $ head corners

    print . product . map tileId $ corners
    print . M.size . M.filter (== 1) . removeMonsters . findTransform $ img
