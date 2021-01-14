{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections #-}

import Advent.Megaparsec
import Data.List (permutations, maximumBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

format :: Parser (String, [(String, Int)])
format = (\a f n b -> (a, [(b, f n)]))
    <$> name <* " would "
    <*> (("gain" $> id) <|> ("lose" $> negate)) <* " "
    <*> decimal <* " happiness units by sitting next to "
    <*> name <* "."
    where name = some letterChar

happiness :: Map String (Map String Int) -> [String] -> Int
happiness hapMap perm = sum . map f . zip [0..] $ perm
    where size    = length perm
          left  i = perm !! ((i - 1) `mod` size)
          right i = perm !! ((i + 1) `mod` size)

          f (i, v) = hapDelta (left i) + hapDelta (right i)
              where hapDelta v' = fromJust . Map.lookup v' . fromJust $ Map.lookup v hapMap

optimalHappiness :: Map String (Map String Int) -> [String] -> Int
optimalHappiness hapmap = maximum . map (happiness hapmap) . permutations

addYou :: Map String (Map String Int) -> [String] -> Map String (Map String Int)
addYou hapmap people = Map.insert "You" youMap hapmap'
    where hapmap' = Map.map (Map.insert "You" 0) hapmap
          youMap  = Map.fromList . map (,0) $ people

-- |
-- >>> :main
-- 618
-- 601
main :: IO ()
main = do
    input <- readParsedLines 2015 13 format

    let hapmap  = Map.map Map.fromList . Map.fromListWith (++) $ input
        people  = Map.keys hapmap

        hapMap2 = addYou hapmap people

    print . optimalHappiness hapmap  $         people
    print . optimalHappiness hapMap2 $ "You" : people
