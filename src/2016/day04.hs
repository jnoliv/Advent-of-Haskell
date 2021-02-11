{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Data.Char (chr, ord)
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Ord (Ordering(LT, EQ, GT))

type Room = (String, Int, String)   -- encrypted name, sector ID, checksum

format :: Parser Room
format = (,,) <$> (concat <$> (many letterChar `endBy` "-"))
              <*> decimal
              <*> ("[" *> many letterChar <* "]")

-- | The checksum is the five most common letters in the encrypted
-- name, in order, with ties broken by alphabetization.
-- >>> checksum "aaaaabbbzyx"
-- "abxyz"
--
-- >>> checksum "notarealroom"
-- "oarel"
checksum :: String -> String
checksum l = take 5 sortedByCount
    where
        counts        = Map.toList $ foldl (\m k -> Map.insertWith (+) k 1 m) Map.empty l
        sortedByCount = map fst . sortBy order $ counts

        -- | Larger counts first, then regular ordering of chars
        order (c, n) (c', n')
            | n > n'    = LT
            | n < n'    = GT
            | otherwise = compare c c'

real :: Room -> Bool
real (name, _, csum) = checksum name == csum

-- | Rotate each letter forward through the alphabet a number
-- of times equal to the room's sector ID.
-- >>> decipher ("qzmtzixmtkozyivhz", 343, "")
-- ("veryencryptedname",343,"")
decipher :: Room -> Room
decipher (name, sector, csum) = (map f name, sector, csum)
    where
        f c = chr $ 97 + (ord c - 97 + sector) `mod` 26

storage :: [Room] -> Room
storage = head . filter (\(a,_,_) -> a == "northpoleobjectstorage")

-- |
-- >>> :main
-- 278221
-- 267
main :: IO ()
main = do
    rooms <- readParsedLines 2016 4 format

    let realRooms      = filter real rooms
        sector (_,b,_) = b

    print . sum $ map sector realRooms
    print . sector . storage $ map decipher realRooms
