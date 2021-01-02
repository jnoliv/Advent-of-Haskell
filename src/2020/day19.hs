{-# Language OverloadedStrings #-}

import AdventAPI (readInputDefaults)
import Advent.Megaparsec
import Advent.Utils (count)
import Control.Monad.State
import qualified Data.IntMap as M
import Data.List (isPrefixOf)

data Rule = Rule [[Int]] | Term String
    deriving Show

type RuleMap = M.IntMap Rule

terminal :: Parser Rule
terminal = Term <$> ("\"" *> many letterChar <* "\"")

subrule :: Parser Rule
subrule = Rule <$> many (decimal <* optional " ") `sepBy` "| "

rule :: Parser (Int, Rule)
rule = (,) <$> (decimal <* ": ") <*> (terminal <|> subrule)

message :: Parser String
message = many $ oneOf ['a','b']

format :: Parser ([(Int, Rule)], [String])
format = (,) <$> (rule `endBy` "\n") <* "\n" <*> (message `endBy` "\n")

-- | Match all of the rules
matchSubrule :: RuleMap -> [Int] -> State [String] Bool
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
matchRule :: RuleMap -> [[Int]] -> State [String] Bool
matchRule _ [] = put [] >> return False
matchRule ruleMap (sr:srs) = do
    toMatchB <- get
    let (success, toMatchA)  = runState (matchSubrule ruleMap sr) toMatchB
        (success', toMatch') = runState (matchRule ruleMap srs) toMatchB

    if success
        then put (toMatchA ++ toMatch') >> return True
        else put toMatch' >> return success'

-- | Match a rule
match :: RuleMap -> Int -> State [String] Bool
match ruleMap ruleId = do
    let Just rule = M.lookup ruleId ruleMap

    toMatch <- get
    case rule of
        Term s -> let matches = filter (s `isPrefixOf`) toMatch
            in if null matches
                then return False
                else put (map tail matches) >> return True
        Rule subrules -> matchRule ruleMap subrules

-- |
-- >>> :main
-- 291
-- 409
main :: IO()
main = do
    input <- readInputDefaults 2020 19
    let (rules, messages) = parseWrapper format input
        ruleMap           = M.fromList rules
        ruleMap2          = M.insert 8 (Rule [[42], [42,8]]) . M.insert 11 (Rule [[42,11,31], [42,31]]) $ ruleMap

        success (b,s)     = b && any null s
        go rm             = print . count id . map (success . runState (match rm 0) . return)

    go ruleMap  messages
    go ruleMap2 messages
