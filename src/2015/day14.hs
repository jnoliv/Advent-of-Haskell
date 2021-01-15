{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

format :: Parser (Int, Int, Int)
format = (,,) <$> (some letterChar *> " can fly " *> decimal <* " km/s for ")
              <*> decimal <* " seconds, but then must rest for "
              <*> decimal <* " seconds."

distance :: Int -> (Int, Int, Int) -> Int
distance time (speed, stamina, rest) = (q * stamina + r') * speed
    where (q,r) = time `divMod` (stamina + rest)
          r'    = min r stamina

winner :: Int -> [(Int, Int, Int)] -> Int
winner time stats = maximum $ Map.elems scores
    where
        lead i = (\l -> fromJust $ (maximum l) `elemIndex` l) $ map (distance i) stats
        scores = foldl (\s t -> Map.insertWith (+) (lead t) 1 s) Map.empty [1 .. time]
          
-- |
-- >>> :main
-- 2655
-- 1059
main :: IO ()
main = do
    input <- readParsedLines 2015 14 format

    print . maximum . map (distance 2503) $ input
    print . winner 2503 $ input
