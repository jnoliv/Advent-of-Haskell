{-# Language OverloadedStrings #-}

import AdventAPI (readInputDefaults)
import Control.Applicative (many, optional, (<|>))
import Control.Monad.State
import qualified Data.IntMap as M
import Text.Megaparsec (endBy, sepBy, oneOf)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (count, Parser, parseWrapper)

data Rule = Rule [[Int]] | Term Char
    deriving Show

type RuleMap = M.IntMap Rule

terminal :: Parser Rule
terminal = Term <$> ("\"" *> letterChar <* "\"")

subrule :: Parser Rule
subrule = Rule <$> many (decimal <* optional " ") `sepBy` "| "

rule :: Parser (Int, Rule)
rule = (,) <$> (decimal <* ": ") <*> (terminal <|> subrule)

message :: Parser String
message = many $ oneOf ['a','b']

format :: Parser ([(Int, Rule)], [String])
format = (,) <$> (rule `endBy` "\n") <* "\n" <*> (message `endBy` "\n")

-- | Match all of the rules
matchSubrule :: RuleMap -> [Int] -> State String Bool
matchSubrule _ [] = return True
matchSubrule ruleMap (r:rs) = do
    toMatch <- get
    success <- match ruleMap r

    if not success
        then return False
        else do
            success' <- matchSubrule ruleMap rs

            if success'
                then return True
                else put toMatch >> return False

-- | Match one of the subrules
matchRule :: RuleMap -> [[Int]] -> State String Bool
matchRule _ [] = return False
matchRule ruleMap (sr:srs) = do
    success <- matchSubrule ruleMap sr

    if success
        then return True
        else matchRule ruleMap srs

-- | Match a rule
match :: RuleMap -> Int -> State String Bool
match ruleMap ruleId = do
    let Just rule = M.lookup ruleId ruleMap

    toMatch <- get
    case rule of
        Term c -> if head toMatch == c
            then put (tail toMatch) >> return True
            else return False
        Rule subrules -> matchRule ruleMap subrules

main :: IO()
main = do
    input <- readInputDefaults 19
    let (rules, messages) = parseWrapper format input
        ruleMap           = M.fromList rules

        success (b,s)     = b && null s

    print . count id . map (success . runState (match ruleMap 0)) $ messages
