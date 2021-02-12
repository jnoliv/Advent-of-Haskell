{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec
import Data.Bifunctor (first, second)
import Data.List (isInfixOf)

data Sequence = Supernet String | Hypernet String
type IP       = [Sequence]

supernets, hypernets :: IP -> [String]
supernets ip = [x | Supernet x <- ip]
hypernets ip = [x | Hypernet x <- ip]

format :: Parser IP
format = some sequence
    where
        sequence = Hypernet <$> ("[" *> some letterChar <* "]")
               <|> Supernet <$> (some letterChar)

hasABBA :: String -> Bool
hasABBA (a : b : c : d : cs)
    | a == d && b == c && a /= b = True
    | otherwise                  = hasABBA $ b : c : d : cs
hasABBA _ = False

allABAs :: String -> [String]
allABAs (a : b : c : cs)
    | a == c && a /= b = [a,b,c] : allABAs (b : c : cs)
    | otherwise        =           allABAs (b : c : cs)
allABAs _ = []

-- |
-- >>> supportsTLS [Supernet "abba", Hypernet "mnop", Supernet "qrst"]
-- True
--
-- >>> supportsTLS [Supernet "abcd", Hypernet "bddb", Supernet "xyyx"]
-- False
--
-- >>> supportsTLS [Supernet "aaaa", Hypernet "qwer", Supernet "tyui"]
-- False
-- 
-- >>> supportsTLS [Supernet "ioxxoj", Hypernet "asdfgh", Supernet "zxcvbn"]
-- True
supportsTLS :: IP -> Bool
supportsTLS ip = any (      hasABBA) (supernets ip)
              && all (not . hasABBA) (hypernets ip)

-- |
-- >>> supportsSSL [Supernet "aba", Hypernet "bab", Supernet "xyz"]
-- True
--
-- >>> supportsSSL [Supernet "xyx", Hypernet "xyx", Supernet "xyx"]
-- False
--
-- >>> supportsSSL [Supernet "aaa", Hypernet "kek", Supernet "eke"]
-- True
--
-- >>> supportsSSL [Supernet "zazbz", Hypernet "bzb", Supernet "cdb"]
-- True
supportsSSL :: IP -> Bool
supportsSSL ip = any matchedABA $ concatMap allABAs (supernets ip)
    where
        matchedABA aba = any (hasBAB aba) (hypernets ip)
        hasBAB [a,b,_] = ([b,a,b] `isInfixOf`)

-- |
-- >>> :main
-- 105
-- 258
main :: IO ()
main = do
    ips <- readParsedLines 2016 7 format

    print . length $ filter supportsTLS ips
    print . length $ filter supportsSSL ips
