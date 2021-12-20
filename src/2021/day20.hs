{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (Parser, asciiChar, char, some, (<|>), readParsed)
import Advent.Coord.Grid (Coord, neighbours8)
import Advent.Utils (readAsMap, readBin)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

format :: Parser (String, String)
format = (,) <$> some (char '.' <|> char '#') <* "\n\n" <*> some asciiChar

-- |
-- >>> :main
-- 
main :: IO ()
main = do
    (input1, input2) <- readParsed 2021 20 format

    let enhancer = map (\c -> if c == '#' then '1' else '0') input1
        image    = readAsMap (\c -> case c of {'#' -> Just '1'; '.' -> Just '0'; _ -> Nothing}) input2
    
        enhanced = iterate (enhance enhancer) (image, '0')

    print . Map.size . Map.filter (== '1') . fst $ enhanced !! 2
    print . Map.size . Map.filter (== '1') . fst $ enhanced !! 50


enhance :: [Char] -> (Map Coord Char, Char) -> (Map Coord Char, Char)
enhance enhancer (image, def) =
    (Set.foldl (\i p -> Map.insert p (enhancePixel p) i) Map.empty pixels, def')
    where
        pixels           = Map.foldlWithKey (\i p _ -> Set.union i $ allThatContain p) Set.empty image
        allThatContain p = Set.fromList $ p : neighbours8 p

        enhancePixel p = enhancer !! readBin bits
            where
                neighboursOrd = sort $ p : neighbours8 p
                bits          = map (\p -> Map.findWithDefault def p image) neighboursOrd
        
        def' = if def == '1' then '0' else '1'
