{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Advent.Utils (showSet)
import Data.Bool (bool)
import Data.Set (Set)
import Data.Set qualified as Set

data Instr = Rect     Int Int   -- width height
           | ShiftRow Int Int   -- row   shift
           | ShiftCol Int Int   -- col   shift
    deriving Show

format :: Parser Instr
format =     (Rect     <$> ("rect "            *> decimal <* "x"   ) <*> decimal)
     <|> try (ShiftRow <$> ("rotate row y="    *> decimal <* " by ") <*> decimal)
     <|>     (ShiftCol <$> ("rotate column x=" *> decimal <* " by ") <*> decimal)

display :: Int -> Int -> [Instr] -> Set (Int,Int)
display height width = foldl applyInstr Set.empty
    where
        applyInstr set (Rect     w h) = Set.union set $ Set.fromList [(r,c) | r <- [0 .. h - 1], c <- [0 .. w - 1]]
        applyInstr set (ShiftRow i n) = Set.map (\(r,c) -> if r == i then (r, (c + n) `mod` width)  else (r,c)) set
        applyInstr set (ShiftCol i n) = Set.map (\(r,c) -> if c == i then ((r + n) `mod` height, c) else (r,c)) set

-- |
-- >>> :main
-- 121
-- ###  #  # ###  #  #  ##  ####  ##  ####  ### #   
-- #  # #  # #  # #  # #  # #    #  # #      #  #   
-- #  # #  # #  # #  # #    ###  #  # ###    #  #   
-- ###  #  # ###  #  # #    #    #  # #      #  #   
-- # #  #  # # #  #  # #  # #    #  # #      #  #   
-- #  #  ##  #  #  ##   ##  ####  ##  ####  ### ####
main :: IO ()
main = do
    instr <- readParsedLines 2016 8 format

    let displaySt = display 6 50 instr

    print $ Set.size displaySt
    putStrLn $ showSet (bool ' ' '#') displaySt
