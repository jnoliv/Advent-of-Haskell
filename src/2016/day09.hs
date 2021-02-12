{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec
import Advent.Utils (count)
import Data.Char (isSpace)

data Chunk = Raw        String
           | Compressed Int Int [Chunk]  -- nChars nReps nestedChunks
type File  = [Chunk]

format :: Parser File
format = some (raw <|> compressed)
    where
        raw        = Raw <$> some (noneOf ['('])
        compressed = do
            nChars <- "(" *> decimal <* "x"
            nReps  <-        decimal <* ")"
            chars  <- parseWrapper format <$> replicateM nChars asciiChar

            return $ Compressed nChars nReps chars

size :: File -> Int
size = sum . map f
    where
        f (Raw x)            = count (not . isSpace) x
        f (Compressed n r _) = n * r

size2 :: File -> Int
size2 = sum . map f
    where
        f (Raw x)            = count (not . isSpace) x
        f (Compressed _ r f) = r * size2 f

-- |
-- >>> :main
-- 123908
-- 10755693147
main :: IO ()
main = do
    file <- readParsed 2016 9 format

    print $ size  file
    print $ size2 file
