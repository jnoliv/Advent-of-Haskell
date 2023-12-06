module Advent.Megaparsec (
    Parser,
    readParsed, readParsedLines, parseLines, parseWrapper,
    sdecimal, trim,

    (<|>),                                              -- Control.Applicative
    replicateM,                                         -- Control.Monad
    ($>),                                               -- Data.Functor

    eof, try, sepBy, sepBy1, endBy, endBy1, optional,   -- Text.Megaparsec
    many, some, manyTill, oneOf, noneOf, lookAhead,     -- Text.Megaparsec

    char, letterChar, alphaNumChar, digitChar,          -- Text.Megaparsec.Char
    hexDigitChar, lowerChar, upperChar, asciiChar,      -- Text.Megaparsec.Char

    decimal                                             -- Text.Megaparsec.Char.Lexer
) where

import Advent.API (readInputDefaults)
import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
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

-- | Trim allows a (imo) more intuitive representation than between.
-- Note that trim prefix p suffix == between prefix suffix p
trim :: Parser pre -> Parser p -> Parser suf -> Parser p
trim prefix p suffix = between prefix suffix p
