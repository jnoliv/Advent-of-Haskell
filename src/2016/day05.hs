{-# LANGUAGE ImportQualifiedPost #-}

import AdventAPI (readInputDefaults)
import Advent.Utils (md5)
import Data.Function (on)
import Data.List (isPrefixOf, nubBy, sort)

firstPositions :: Int -> [String] -> String
firstPositions len hashes = map snd . sort . take len . nub . toPosVal $ filter valid hashes
    where
        valid h  = (h !! 5) `elem` (take len ['0'..])
        toPosVal = map ((,) <$> (!! 5) <*> (!! 6))
        nub      = nubBy ((==) `on` fst)

-- |
-- >> :main
-- "1a3099aa"
-- "694190cd"
main :: IO ()
main = do
    doorID <- init <$> readInputDefaults 2016 5

    let hashes = filter ("00000" `isPrefixOf`) . map (md5 . (doorID ++) . show) $ [0..]

    putStrLn . take 8 $ map (!! 5) hashes
    putStrLn $ firstPositions 8 hashes
