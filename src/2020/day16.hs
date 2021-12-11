{-# Language OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Data.Bifunctor (first)
import Data.List (delete, transpose, isPrefixOf, sortOn)

type Ticket = [Int]

data Rule = Rule { name :: String, l1 :: Int, r1 :: Int, l2 :: Int, r2 :: Int }
    deriving (Eq, Show)

rulesF :: Parser Rule
rulesF = Rule <$> some (letterChar <|> char ' ') <* ": "
              <*> decimal <* "-" <*> decimal <* " or "
              <*> decimal <* "-" <*> decimal

ticketF :: Parser Ticket
ticketF = decimal `sepBy` ","

format :: Parser ([Rule], Ticket, [Ticket])
format =
    (,,) <$>  rulesF `endBy` "\n"  <* "\nyour ticket:\n"
         <*> ticketF               <* "\n\nnearby tickets:\n"
         <*> ticketF `endBy` "\n"

-- | Check if any field of the ticket fails all rules.
-- Also returns the sum of the invalid fields
checkTicket :: [Rule] -> Ticket -> (Bool, Int)
checkTicket rules ticket = (null invalidFields, sum invalidFields)
    where invalidFields = [field | field <- ticket, not $ any (`checkRule` field) rules]

-- | Check if the field passes the rule
checkRule :: Rule -> Int -> Bool
checkRule (Rule _ l1 r1 l2 r2) n =
    (l1 <= n && n <= r1) || (l2 <= n && n <= r2)

-- | Find all valid rules for a list of fields (not a ticket,
-- a list of a field in all tickets)
findValidRules :: [(Rule, Int)] -> [Int] -> [Int]
findValidRules rulesIndexed fields = [i | (r, i) <- rulesIndexed, all (checkRule r) fields]

-- | Reorder the rules ascending by length of possibilities, keeping the
-- original index. Apply the greedy algorithm and then reorder using said
-- index. Drop the indexes and return the order of the rules
findRuleOrder :: [[Int]] -> [Int]
findRuleOrder rules = map fst . sortOn snd . greedy $ sz
    where sz = sortOn (length . fst) $ zip rules [0..] :: [([Int], Int)]

-- | By printing 'validRules' it is clear that by fixing the rule to the
-- column that only has one possible rule, this process can then be repeated
-- finding the ordering greedily  
greedy :: [([Int], Int)] -> [(Int, Int)]
greedy []           = []
greedy (rule:rules) = (choice, snd rule) : (greedy . map (first (delete choice)) $ rules)
    where choice = head . fst $ rule

-- |
-- >>> :main
-- 27850
-- 491924517533
main :: IO()
main = do
    (rules, ownTicket, tickets) <- parseWrapper format <$> readInputDefaults 2020 16

    let validTickets = filter (fst . checkTicket rules) tickets
        validRules   = map (findValidRules $ zip rules [0..]) $ transpose validTickets
        rulesInOrder = map (rules !!) $ findRuleOrder validRules
        prod         = product . map fst . filter (("departure" `isPrefixOf`) . name . snd) . zip ownTicket $ rulesInOrder

    print . sum . map (snd . checkTicket rules) $ tickets
    print prod
