{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Advent.Coord.Grid
import Advent.Life
import Advent.Math
import Advent.Utils

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe

import Data.List.Split

data Inst = Noop | Addx Integer
    deriving Show

format :: Parser Inst
format = (Noop <$ "noop")
     <|> (Addx <$> ("addx " *> sdecimal))

exec :: [Inst] -> [(Integer, Integer)]
exec = zip [1..] . reverse . foldl execInst [1]

execInst :: [Integer] -> Inst -> [Integer]
execInst xs@(x : _) inst =
    case inst of
        Noop   ->         x : xs
        Addx n -> x + n : x : xs

inSprite :: (Integer, Integer) -> Bool
inSprite (cycle, x) = x - 1 <= px && px <= x + 1
    where px = (cycle - 1) `mod` 40

toPixel :: Bool -> Char
toPixel True  = '#'
toPixel False = ' '

-- |
-- >>> :main
-- 11720
-- #### ###   ##  ###  #### ###   ##    ## 
-- #    #  # #  # #  # #    #  # #  #    # 
-- ###  #  # #    #  # ###  #  # #       # 
-- #    ###  #    ###  #    ###  #       # 
-- #    # #  #  # # #  #    #    #  # #  # 
-- #### #  #  ##  #  # #### #     ##   ##  
main :: IO ()
main = do
    instructions <- readParsedLines 2022 10 format

    let xs      = exec instructions
        signals = map (uncurry (*) . head) . chunksOf 40 . drop 19 $ xs

        pixels  = chunksOf 40 . map (toPixel . inSprite) . init $ xs

    print $ sum signals
    mapM_ putStrLn pixels
