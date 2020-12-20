import AdventAPI (readInputDefaults)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Utils (readAsMap, binToDec, readBin, showBin, count)

type ImageData = M.Map (Int,Int) Int

data Tile = Tile { tid           :: Integer,
                   image         :: ImageData,
                   up            :: Int,
                   right         :: Int,
                   down          :: Int,
                   left          :: Int,
                   borderMatches :: Int
            } deriving Show

borders :: Tile -> [Int]
borders tile = map ($ tile) [up, right, down, left]

parseInput :: String -> [(Integer, ImageData)]
parseInput = map parseTile . splitOn "\n\n"

parseTile :: String -> (Integer, ImageData)
parseTile tile = (tid, tileMap)
    where tid     = read . take 4 . drop 5 $ tile
          tileMap = readAsMap (\c -> if c == '#' then Just 1 else Just 0) $ drop 11 tile

-- | Invert the border. For example, if the top border of a tile
-- will match the top border of another tile after 180º rotation,
-- the representation of the border will be different. This computes
-- that new representation.
invert :: Int -> Int -> Int
invert n b = readBin . reverse $ bin'
    where bin = showBin b
          bin' = replicate (n - length bin) '0' ++ bin

-- | Encode each border as a binary number
encodeBorders :: Int -> (Integer, ImageData) -> Tile
encodeBorders size (tid, tile) = Tile tid tile upB downB rightB leftB 0
    where getBorder f = binToDec . map snd . M.toAscList . M.filterWithKey f $ tile
          upB    = getBorder (\k _ -> fst k == 0)
          downB  = getBorder (\k _ -> fst k == size - 1)
          rightB = getBorder (\k _ -> snd k == size - 1)
          leftB  = getBorder (\k _ -> snd k == 0)

-- | This could very much be optimized, but it doesn't matter right now
countBorderMatches :: Int -> [Tile] -> [Tile] -> [Tile]
countBorderMatches _ _ [] = []
countBorderMatches tileSize allTiles (curTile:tiles) =
    curTile { borderMatches = bm } : countBorderMatches tileSize allTiles tiles
    where matchBorder b = count (==b) . concatMap borders . filter ((/= tid curTile) . tid) $ allTiles
          bm = sum . map matchBorder $ borders curTile ++ map (invert tileSize) (borders curTile)


main :: IO()
main = do
    input <- readInputDefaults 20
    let tiles     = parseInput input
        tileSize  = round . sqrt . fromIntegral . M.size . snd . head $ tiles
        encTiles' = map (encodeBorders tileSize) tiles
        encTiles  = countBorderMatches tileSize encTiles' encTiles'

    print . product . map tid . filter ((== 2) . borderMatches) $ encTiles
