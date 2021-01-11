{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec

data Character = Backslash | Quote | Hex Char Char | Character Char

format :: Parser [Character]
format = "\"" *> many character <* "\""
    where character = ("\\\\" $>  Backslash)
                  <|> ("\\\"" $>  Quote)
                  <|> ("\\x"  *> (Hex <$> hexDigitChar <*> hexDigitChar))
                  <|> (Character <$> noneOf ['\"'])

sizeDiff :: [Character] -> Int
sizeDiff = (+ 2) . sum . map characterDiff -- 2 extra for the quotes
    where 
        characterDiff  Backslash    = 1     -- \\   -> \
        characterDiff  Quote        = 1     -- \"   -> "
        characterDiff (Hex _ _)     = 3     -- \xDD -> c
        characterDiff (Character _) = 0     -- c    -> c

sizeDiff2 :: [Character] -> Int
sizeDiff2 = (+ 4) . sum . map characterDiff -- 4 extra for the quotes
    where 
        characterDiff  Quote        = 2     -- \\    -> \\\\
        characterDiff  Backslash    = 2     -- \"    -> \\\"
        characterDiff (Hex _ _)     = 1     -- \xDD  -> \\xDD
        characterDiff (Character _) = 0     -- c     -> c

-- |
-- >>> :main
-- 1333
-- 2046
main :: IO ()
main = do
    input <- readParsedLines 2015 8 format

    print . sum . map sizeDiff  $ input
    print . sum . map sizeDiff2 $ input
