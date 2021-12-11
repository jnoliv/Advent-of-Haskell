{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Data.Bifunctor (second)

data Json = Array  [Json]
          | Object [(String, Json)]
          | String String
          | Number Int
    deriving (Show, Eq)

format :: Parser Json
format = array <|> object <|> string <|> number
    where
        array  = Array  <$> ("[" *> format `sepBy` "," <* "]")
        object = Object <$> ("{" *> ((,) <$> quotedStr <* ":" <*> format) `sepBy` "," <* "}")
        string = String <$> quotedStr
        number = Number <$> sdecimal

        quotedStr = "\"" *> some letterChar <* "\""

sumJson :: Json -> Int
sumJson (Array  a) = sum . map sumJson         $ a
sumJson (Object l) = sum . map (sumJson . snd) $ l
sumJson (String s) = 0
sumJson (Number n) = n

removeRed :: Json -> Json
removeRed (Object l)
    | any (== (String "red")) (map snd l) = Object []
    | otherwise                           = Object $ map (second removeRed) l
removeRed (Array a) = Array $ map removeRed a
removeRed json      = json

-- |
-- >>> :main
-- 191164
-- 87842
main :: IO ()
main = do
    input <- parseWrapper format <$> readInputDefaults 2015 12

    print . sumJson             $ input
    print . sumJson . removeRed $ input
