module Advent.Megaparsec (
    Parser, readParsed, readParsedLines, parseLines, parseWrapper, sdecimal,

    (<|>),                                                                                      -- Control.Applicative
    replicateM,                                                                                 -- Control.Monad
    ($>),                                                                                       -- Data.Functor
    try, sepBy, endBy, many, optional, some, oneOf, noneOf, manyTill, lookAhead,                -- Text.Megaparsec
    char, string, letterChar, alphaNumChar, hexDigitChar, lowerChar, upperChar, asciiChar,      -- Text.Megaparsec.Char
    decimal, signed                                                                             -- Text.Megaparsec.Char.Lexer
) where

import AdventAPI (readInputDefaults)
import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, eof, try, lookAhead, sepBy, endBy, many, manyTill, optional, some, oneOf, noneOf)
import Text.Megaparsec.Char (char, string, letterChar, alphaNumChar, hexDigitChar, lowerChar, upperChar, asciiChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser a = Parsec Void String a

-- | Read the input of the given day and apply the given
-- parser to it
readParsed :: Int -> Int -> Parser a -> IO a
readParsed year day parser =
    parseWrapper parser <$> readInputDefaults year day

-- | Read the input of the given day and apply the given
-- parser to all lines of said input
readParsedLines :: Int -> Int -> Parser a -> IO [a]
readParsedLines year day parser =
    parseLines parser <$> readInputDefaults year day

-- | Apply the given parser to all lines of said input
parseLines :: Parser a -> String -> [a]
parseLines parser input = parseWrapper parserFull input
    where parserFull = parser `endBy` char '\n' <* eof

-- | Apply the given parser to the given input. Manages
-- error reporting.
parseWrapper :: Parser a -> String -> a
parseWrapper parser input =
    case parse parser "" input of
        Left e  -> error $ "\n" ++ errorBundlePretty e
        Right r -> r

-- | Read a signed decimal without any whitespace between
-- the sign and the number
sdecimal :: Num a => Parser a
sdecimal = signed (return ()) decimal
