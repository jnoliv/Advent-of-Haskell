{-# Language OverloadedStrings #-}

import AdventAPI (readInputDefaults)
import Advent.Utils (Parser, parseLines)
import Control.Applicative ((<|>))
import Text.Megaparsec (try, oneOf)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

data ExpTree = Exp ExpTree Char ExpTree | Term Integer

chainl1 :: Parser ExpTree -> Parser Char -> Parser ExpTree
chainl1 p op = p >>= rest
    where rest x = try (rest =<< Exp x <$> (" " *> op <* " ") <*> p)
               <|> return x

term :: Parser ExpTree -> Parser ExpTree
term exp = "(" *> exp <* ")"
       <|> Term <$> decimal

expression :: Parser ExpTree
expression = term expression `chainl1` oneOf ['*','+']

expression' :: Parser ExpTree
expression' = (term expression' `chainl1` char '+') `chainl1` char '*'

evaluate :: ExpTree -> Integer
evaluate (Term n) = n
evaluate (Exp e1 '+' e2) = evaluate e1 + evaluate e2
evaluate (Exp e1 '*' e2) = evaluate e1 * evaluate e2

main :: IO()
main = do
    input <- readInputDefaults 2020 18
    
    let go p = print . sum . map evaluate . parseLines p $ input
    mapM_ go [expression, expression']
