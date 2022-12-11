{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (Parser, readParsedLines, try, (<|>), many, letterChar, decimal)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

data Wire  = Wire String | Signal Int
data Instr = In  Wire
           | Not Wire
           | Or  Wire Wire
           | And Wire Wire
           | LShift Wire Int
           | RShift Wire Int

format :: Parser (String, Instr)
format = flip (,) <$> instr <*> (" -> " *> many letterChar)
    where
        wire  = (Signal <$> decimal)
            <|> (Wire   <$> many letterChar)
        instr = try (Or     <$> wire <* " OR "     <*> wire)
            <|> try (And    <$> wire <* " AND "    <*> wire)
            <|> try (LShift <$> wire <* " LSHIFT " <*> decimal)
            <|> try (RShift <$> wire <* " RSHIFT " <*> decimal)
            <|>     (Not    <$> ("NOT " *> wire))
            <|>     (In     <$> wire)

process :: Map String Instr -> String -> Int
process instrs finalWire = go finalWire
    where
        go wire = case Map.lookup wire instrs of
            Just (In  w)         -> processWire w
            Just (Not w)         -> (65535 -) $ processWire w
            Just (Or      w1 w2) -> (processWire w1) .|. (processWire w2)
            Just (And     w1 w2) -> (processWire w1) .&. (processWire w2)
            Just (LShift  w n)   -> shiftL (processWire w) n
            Just (RShift  w n)   -> shiftR (processWire w) n
        
        processWire (Signal n) = n
        processWire (Wire   w) = fromJust $ Map.lookup w wireOutputs

        wireOutputs = Map.fromList [(w, go w) | w <- Map.keys instrs]

-- |
-- >>> :main
-- 16076
-- 2797
main :: IO ()
main = do
    instrs <- Map.fromList <$> readParsedLines 2015 7 format

    let out1    = process instrs "a"
        instrs' = Map.insert "b" (In (Signal out1)) instrs

    print $ out1
    print $ process instrs' "a"
