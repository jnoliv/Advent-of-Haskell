{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Data.Char (digitToInt, isDigit)
import Data.Text qualified as Text

toNum :: String -> Int
toNum s = read [head s, last s]

replaceSpelled :: String -> String
replaceSpelled s@('o' : 'n' : 'e' : t)             = '1' : replaceSpelled (tail s)
replaceSpelled s@('t' : 'w' : 'o' : t)             = '2' : replaceSpelled (tail s)
replaceSpelled s@('t' : 'h' : 'r' : 'e' : 'e' : t) = '3' : replaceSpelled (tail s)
replaceSpelled s@('f' : 'o' : 'u' : 'r' : t)       = '4' : replaceSpelled (tail s)
replaceSpelled s@('f' : 'i' : 'v' : 'e' : t)       = '5' : replaceSpelled (tail s)
replaceSpelled s@('s' : 'i' : 'x' : t)             = '6' : replaceSpelled (tail s)
replaceSpelled s@('s' : 'e' : 'v' : 'e' : 'n' : t) = '7' : replaceSpelled (tail s)
replaceSpelled s@('e' : 'i' : 'g' : 'h' : 't' : t) = '8' : replaceSpelled (tail s)
replaceSpelled s@('n' : 'i' : 'n' : 'e' : t)       = '9' : replaceSpelled (tail s)
replaceSpelled (c : t)                             =  c  : replaceSpelled t
replaceSpelled []                                  = []

-- |
-- >>> :main
-- 54953
-- 53868
main :: IO ()
main = do
    input <- lines <$> readInputDefaults 2023 1

    let ns = map (toNum . filter isDigit) input
    print $ sum ns

    let ns' = map (toNum . filter isDigit . replaceSpelled) input
    print $ sum ns'
