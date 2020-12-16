import AdventAPI (readInputDefaults)
import Control.Applicative (many)
import Data.List (delete, transpose, isPrefixOf, notElem)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Megaparsec (sepBy, manyTill)
import Text.Megaparsec.Char (char, string, asciiChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (Parser, parseLines)

import Debug.Trace (trace)

type Ticket = [Int]

data Rule = Rule { name :: String, l1 :: Int, r1 :: Int, l2 :: Int, r2 :: Int }
    deriving (Eq, Show)

formatRules :: Parser Rule
formatRules = Rule <$> manyTill asciiChar (char ':') <* char ' '
                   <*> decimal <* char '-'
                   <*> decimal <* string " or "
                   <*> decimal <* char '-'
                   <*> decimal

formatTicket :: Parser Ticket
formatTicket = decimal `sepBy` char ','

-- | Check if any field of the ticket fails all rules.
-- Also returns the sum of the invalid fields
checkTicket :: [Rule] -> Ticket -> (Bool, Int)
checkTicket rules ticket = (null failedFields, sum failedFields) 
    where checkField rules n = if any (`checkRule` n) rules then Nothing else Just n
          failedFields       = mapMaybe (checkField rules) ticket

-- | Check if the field passes the rule
checkRule :: Rule -> Int -> Bool
checkRule (Rule _ l1 r1 l2 r2) n =
    (l1 <= n && n <= r1) || (l2 <= n && n <= r2)

-- | Find all valid rules for a list of fields
findValidRules :: [(Rule, Int)] -> [Int] -> [Int]
findValidRules rulesIndexed fields = [i | (r, i) <- rulesIndexed, all (checkRule r) fields]

-- |
findRuleOrder :: Int -> [[Int]] -> [Int] -> [Int]
findRuleOrder _      []     chosen = chosen
findRuleOrder nRules (r:rs) chosen = if null validOrderings then [] else head validOrderings
    where availableRules = filter (`notElem` chosen) r
          allOrderings   = map (findRuleOrder nRules rs . (chosen ++) . return) availableRules
          validOrderings = filter ((== nRules) . length) allOrderings

main :: IO()
main = do
    [rulesR, ownTicketR, ticketsR] <- splitOn "\n\n" <$> AdventAPI.readInputDefaults 16

    let rules        = parseLines formatRules $ rulesR ++ "\n"
        ownTicket    = head . parseLines formatTicket . tail . dropWhile (/= '\n') $ ownTicketR ++ "\n"
        tickets      = parseLines formatTicket . tail . dropWhile (/= '\n') $ ticketsR

        validTickets = filter (fst . checkTicket rules) tickets
        validRules   = map (findValidRules $ zip rules [0..]) $ transpose validTickets
        ruleOrder    = findRuleOrder (length rules) validRules []
        prod         = product . map fst . filter (("departure" `isPrefixOf`) . name . snd) . zip ownTicket $ map (rules !!) ruleOrder

    print . sum . map (snd . checkTicket rules) $ tickets
    print prod
