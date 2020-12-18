{-# Language OverloadedStrings #-}

import Utils (readParsedLines, Parser)
import Control.Applicative ((<|>))
import Text.Megaparsec.Char (asciiChar)
import Text.Megaparsec.Char.Lexer (decimal)

data ExpTree = Exp ExpTree Char ExpTree | Term Int
    deriving (Show)

term :: Parser ExpTree
term = "(" *> expression <* ")"
     <|> Term <$> decimal

expression :: Parser ExpTree
expression = do { x <- term; rest x }
    where rest x = do { op <- " " *> asciiChar <* " "
                      ; y  <- term
                      ; rest $ Exp x op y
                      }
                    <|> return x

evaluate :: ExpTree -> Int
evaluate (Term n) = n
evaluate (Exp e1 op e2) =
    case op of
        '+' -> evaluate e1 + evaluate e2
        '*' -> evaluate e1 * evaluate e2

main :: IO()
main = do
    expressions <- readParsedLines 18 expression

    print . sum . map evaluate $ expressions
