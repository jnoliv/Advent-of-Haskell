-- | This module contains input / output and parsing utilities.
module Advent.IO (
    -- * Reading input
    readInput,

    -- * Parser combinators
    Parser,
    -- ** Parsing
    parse, parseLines, parseInput, parseInputLines,
    -- ** Combinators
    sdecimal, trim,
    -- ** Re-exports
    -- $reexports
    decimal,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    (<|>), ($>), replicateM,

    -- * Binary
    readBin, showBin,
) where

import Control.Applicative ((<|>), empty)
import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS (pack, unpack)
import Data.Char (intToDigit, digitToInt)
import Data.Functor (($>))
import Data.Text qualified as Text (pack)
import Data.Void (Void)
import Numeric (showIntAtBase, readInt)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>), (<.>))

import Network.HTTP.Req (
        (/:),
        bsResponse,
        defaultHttpConfig,
        header,
        https,
        req,
        responseBody,
        runReq,
        NoReqBody(NoReqBody),
        GET(GET)
    )

import Text.Megaparsec qualified as Megaparsec (Parsec, between, eof, errorBundlePretty, parse)
import Text.Megaparsec (
        some, many, manyTill, oneOf, noneOf,
        sepBy, sepBy1, endBy, endBy1,
        try,
    )
import Text.Megaparsec.Char (
        alphaNumChar, letterChar, digitChar, hexDigitChar,
        punctuationChar, asciiChar,
        lowerChar, upperChar,
    )
import Text.Megaparsec.Char.Lexer (decimal, signed)

{- $reexports
This module re-exports commonly used combinators from several modules:

* "Control.Applicative"
* "Control.Monad"
* "Data.Functor"
* "Text.Megaparsec"
* "Text.Megaparsec.Char"
* "Text.Megaparsec.Char.Lexer"
-}

{- | Reads input for the given year and day. If the input file already exists,
its entire contents are read and returned. Otherwise, the input is fetched
from the Advent of Code servers and saved to a file.

Requires a session-cookie.txt file to exist containing a valid AoC session cookie.

> readInput 2023 15
-}
readInput :: Int -> Int -> IO String
readInput year day = do
    let filename = "input" </> show year </> "input" ++ show day <.> ".txt"
        
    hasFile <- doesFileExist filename
    if hasFile
        then readFile filename
        else fetchAndSaveInput year day "session-cookie.txt" filename

-- | Read the session cookie from given file, fetch the input from servers,
-- save it to a file, and return its contents.
fetchAndSaveInput :: Int -> Int -> FilePath -> FilePath -> IO String
fetchAndSaveInput year day sessionCookiePath filename = do
    sessionCookie <- readFile sessionCookiePath
    contents      <- runInputRequest year day sessionCookie

    writeFile filename contents
    return contents

-- | Get the input from the server using the given session cookie.
runInputRequest :: Int -> Int -> String -> IO String
runInputRequest year day sessionCookie = runReq defaultHttpConfig $ do
    let toText = Text.pack . show

    resp <- req GET
            (https "adventofcode.com" /: toText year /: "day" /: toText day /: "input")
            NoReqBody
            bsResponse -- strict ByteString response
            $ header "Cookie" (BS.pack $ "session=" ++ sessionCookie)
    return $ BS.unpack (responseBody resp)

-- | Alias for
--
-- @'Parsec' 'Void' 'String' return@
type Parser return = Megaparsec.Parsec Void String return

{- | Apply the given parser to the given input, reporting errors when appropriate.

>>> parse (decimal `sepBy` ",") "1,2,3"
[1,2,3]
-}
parse :: Parser a -> String -> a
parse parser input =
    case Megaparsec.parse parser "" input of
        Left  e -> error ("\n" ++ Megaparsec.errorBundlePretty e)
        Right r -> r

{- | Apply the given parser to each line of given input and return a list of results.

>>> parse ((,) <$> decimal <* "," <*> decimal) "(1,2)\n(3,4)\n"
[(1,2),(3,4)]
-}
parseLines :: Parser a -> String -> [a]
parseLines parser = parse (parser `endBy` "\n" <* Megaparsec.eof)

{- | Read the input of the given year and day, and apply the given parser to it.

> parseInput 2023 15 (some letterChar `endBy` ";") input 
-}
parseInput :: Int -> Int -> Parser a -> IO a
parseInput year day parser = 
    parse parser <$> readInput year day

{- | Read the input of the given year and day, and apply the given parser to each line.

> parseInputLines 2023 15 (many hexDigitChar) input 
-}
parseInputLines :: Int -> Int -> Parser a -> IO [a]
parseInputLines year day parser =
    parseLines parser <$> readInput year day

{- | Parse a signed decimal.

==== __Examples__
>>> parse sdecimal "-3"
-3
>>> parse sdecimal "+7"
7
>>> parse sdecimal "1"
1
-}
sdecimal :: Num a => Parser a
sdecimal = signed empty decimal

{- | Parse a list of signed decimals with separator being one of
space, tab or comma.

==== __Examples__
>>> parse sdecimals "+7 1,-3\t0"
[7,1,-3,0]
-}
sdecimals :: Num a => Parser [a]
sdecimals = sdecimal `sepBy` oneOf [' ', '\t', ',']

{- | Identical to between with the last two arguments flipped.

@trim prefix parser suffix ≡ 'between' prefix suffix parser@

==== __Examples__
>>> parse (trim "{" decimal "}") "{3}"
3
-}
trim :: Parser pre -> Parser p -> Parser suf -> Parser p
trim prefix = flip (Megaparsec.between prefix)

{- | Read a binary number.

==== __Examples__
>>> readBin "10101"
21
-}
readBin :: String -> Int
readBin = fst . head . readInt 2 (`elem` ("01" :: String)) digitToInt

{- | Show a number in binary.

==== __Examples__
>>> showBin 21
"10101"
-}
showBin :: Int -> String
showBin n = showIntAtBase 2 intToDigit n ""
