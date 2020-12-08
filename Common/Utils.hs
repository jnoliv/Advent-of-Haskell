module Common.Utils (
    count, xor,
    Parser, readParsedLines
    ) where

import Common.AdventAPI (readInputDefaults)

import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, eof, endBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty)

-- | Count occurences in the given list that satisfy 'cond'
count :: (a -> Bool) -> [a] -> Int
count cond = length . filter cond

-- | Boolean exclusive OR
xor :: Bool -> Bool -> Bool
xor = (/=)


---- Parsing abstractions

type Parser a = Parsec Void String a

-- | Read the input of the given day and apply the given
-- parser to all lines of said input
readParsedLines :: Int -> Parser a -> IO [a]
readParsedLines day parser = do
    input <- readInputDefaults day

    let parserFull = parser `endBy` char '\n' <* eof

    case parse parserFull "" input of
        Left e  -> fail $ "\n" ++ errorBundlePretty e
        Right r -> return r
