import AdventAPI (readInputDefaults)
import Control.Applicative (many)
import Data.List.Split (splitOn)
import Text.Megaparsec (sepBy, manyTill)
import Text.Megaparsec.Char (char, string, asciiChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (Parser, parseLines)

type Ticket = [Int]

data Rule = Rule { name :: String, l1 :: Int, r1 :: Int, l2 :: Int, r2 :: Int }
    deriving (Show)

formatRules :: Parser Rule
formatRules = Rule <$> manyTill asciiChar (char ':') <* char ' '
                   <*> decimal <* char '-'
                   <*> decimal <* string " or "
                   <*> decimal <* char '-'
                   <*> decimal

formatTicket :: Parser Ticket
formatTicket = decimal `sepBy` char ','

-- | Check if any field of the ticket fails all rules
checkTicket :: [Rule] -> Ticket -> Int
checkTicket rules = sum . map (checkField rules)
    where checkField rules n = if any (`checkRule` n) rules then 0 else n

-- | Check if the field passes the rule
checkRule :: Rule -> Int -> Bool
checkRule (Rule _ l1 r1 l2 r2) n =
    (l1 <= n && n <= r1) || (l2 <= n && n <= r2)

main :: IO()
main = do
    [rulesR, ownTicketR, ticketsR] <- splitOn "\n\n" <$> AdventAPI.readInputDefaults 16

    let rules     = parseLines formatRules $ rulesR ++ "\n"
        --ownTicket = head . parseLines formatTicket . tail . dropWhile (/= '\n') $ ownTicketR ++ "\n"
        tickets   = parseLines formatTicket . tail . dropWhile (/= '\n') $ ticketsR

    print . sum . map (checkTicket rules) $ tickets
