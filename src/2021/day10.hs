import Advent.API (readInputDefaults)
import Data.List (sort)

data ParseError2 = Unexpected Char | Incomplete String
    deriving (Show)

openers, closers :: String
openers = "([{<"
closers = ")]}>"

match :: Char -> Char
match '(' = ')'
match '[' = ']'
match '{' = '}'
match '<' = '>'

-- |
-- >>> :main
-- 387363
-- 4330777059
main :: IO ()
main = do
    chunks <- lines <$> readInputDefaults 2021 10

    let parsed     = map parse chunks
        corrupted  = [c | Unexpected c <- parsed]
        incomplete = [s | Incomplete s <- parsed]

        completionScores = sort $ map completionScore incomplete

    print . sum $ map syntaxErrorScore corrupted
    print $ (\l -> l !! (length l `div` 2)) completionScores

-- | Parses by maintaining a stack of open chunks and making
-- sure that closers match the top of  the stack. If an unexpected
-- character is found, it is returned wrapped in Unexpected. If the
-- end of the string is reached with openers in the stack, the match
-- of each opener (i.e. the corresponding closer) in the stack is
-- returned.
-- >>> parse "(]"
-- Unexpected ']'
--
-- >>> parse "{()()()>"
-- Unexpected '>'
--
-- >>> parse "[({(<(())[]>[[{[]{<()<>>"
-- Incomplete "}}]])})]"
-- 
-- >>> parse "{<[[]]>}<{[{[{[]{()[[[]"
-- Incomplete "]]}}]}]}>"
parse :: String -> ParseError2
parse = f []
    where
        f stack     ""      = Incomplete $ map match stack
        f (s:stack) (c:str)
            | c `elem` openers                 = f (c:s:stack) str
            | c `elem` closers && c == match s = f stack str
            | otherwise                        = Unexpected c
        f []        (c:str)
            | c `elem` openers = f [c] str
            | otherwise        = Unexpected c

syntaxErrorScore :: Char -> Int
syntaxErrorScore ')' = 3
syntaxErrorScore ']' = 57
syntaxErrorScore '}' = 1197
syntaxErrorScore '>' = 25137

-- | The completion score is calculated character by character
-- starting with a score of 0. For each character, the score
-- is multiplied by 5 and then the character score is added.
-- >>> completionScore "])}>"
-- 294
--
-- >>> completionScore "}}]])})]"
-- 288957
--
-- >>> completionScore "}}>}>))))"
-- 1480781
completionScore :: String -> Int
completionScore = foldl (\a c -> 5 * a + f c) 0
    where
        f ')' = 1
        f ']' = 2
        f '}' = 3
        f '>' = 4
