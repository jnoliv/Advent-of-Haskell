{-# LANGUAGE OverloadedStrings, BlockArguments, BinaryLiterals #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (readParsed, letterChar, decimal, endBy, sepBy, Parser)
import Advent.Utils (readBin, showBin, takeUntil)
import Data.Bits ((.|.), xor, shiftL, (.&.))
import Data.List (intercalate)
import Data.Maybe (catMaybes)

type Registers = [Int]
type Program   = [Int]

parser :: Parser (Registers,Program)
parser = (,) <$> registers <* "\n" <*> program
    where
        registers = ("Register " *> letterChar *> ": " *> decimal) `endBy` "\n"
        program   = "Program: " *> (decimal `sepBy` ",")

run :: Program -> Registers -> Int -> [Int]
run prog [ra,rb,rc] ip
    | ip >= length prog = []
    | otherwise         =
        case instr of
            0 -> run prog [ra `div` (2^combo), rb, rc] (ip + 2)     -- adv
            1 -> run prog [ra, rb `xor` op, rc] (ip + 2)            -- bxl
            2 -> run prog [ra, combo `mod` 8, rc] (ip + 2)          -- bst
            3 -> if ra == 0                                         -- jnz
                then run prog [ra, rb, rc] (ip + 2)
                else run prog [ra, rb, rc] op
            4 -> run prog [ra, rb `xor` rc, rc] (ip + 2)            -- bxc
            5 -> (combo `mod` 8) : run prog [ra, rb, rc] (ip + 2)   -- out
            6 -> run prog [ra, ra `div` (2^combo), rc] (ip + 2)     -- bdv
            7 -> run prog [ra, rb, ra `div` (2^combo)] (ip + 2)     -- cdv
        where
            (instr, op) = (prog !! ip, prog !! (ip + 1))

            combo = case op of
                4 -> ra
                5 -> rb
                6 -> rc
                7 -> error "reserved combo operand 7"
                _ -> op

decompile :: Program -> Registers -> Int -> String
decompile prog rs@[ra,rb,rc] ip = show (map showBin rs) ++ "\t ip(" ++ show ip ++"): " ++ instrStr
    where
        (instr, op) = (prog !! ip, prog !! (ip + 1))

        comboStr = case op of
            4 -> "[A=" ++ showBin ra ++ "]"
            5 -> "[B=" ++ showBin rb ++ "]"
            6 -> "[C=" ++ showBin rc ++ "]"
            7 -> error "reserved combo operand 7"
            _ -> show op

        instrStr = (!! instr) [
                "adv (A = A / 2^combo)  " ++ comboStr,
                "bxl (B = B xor op)     " ++ show op,
                "bst (B = combo % 8)    " ++ comboStr,
                "jnz (jump if not zero) " ++ comboStr,
                "bxc (B = B xor C)      " ++ comboStr,
                "out (print combo % 8)  " ++ comboStr,
                "bdv (B = A / 2^combo)  " ++ comboStr,
                "cdv (C = A / 2^combo)  " ++ comboStr
            ]

findA :: Program -> [Int]
findA prog = map fst $ foldl1 merge adjusted
    where
        valid    = [filter isValid [fromLow out aLow | aLow <- [0..7]] | out <- prog]
        adjusted = zipWith adjust [0,3..] valid

        fromLow :: Int -> Int -> (Int,Int,Int)
        fromLow out aLow = (aHigh, aLow, b2)
            where
                b3 = out  `xor` 0b110
                b2 = aLow `xor` 0b001
                c  = b2   `xor` b3

                aHigh = c `shiftL` b2

        isValid :: (Int,Int,Int) -> Bool
        isValid (aHigh, aLow, shift) = (aHigh .&. mask) == (aLow .&. mask)
            where
                maskLow  = 0b111
                maskHigh = 0b111 `shiftL` shift

                mask = maskHigh .&. maskLow

        adjust :: Int -> [(Int,Int,Int)] -> [(Int,Int)]
        adjust i = map (adjustPair i)

        adjustPair :: Int -> (Int,Int,Int) -> (Int,Int)
        adjustPair i (high, low, shift) = ((high .|. low) `shiftL` i, mask `shiftL` i)
            where
                mask = (0b111 `shiftL` shift) .|. 0b111

        merge :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
        merge as bs = catMaybes [mergePair a b | a <- as, b <- bs]

        mergePair :: (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
        mergePair (a1, mask1) (a2, mask2)
            | masked1 == masked2 = Just (a1 .|. a2, mask1 .|. mask2)
            | otherwise = Nothing
            where
                masked1 = a1 .&. mask1 .&. mask2
                masked2 = a2 .&. mask1 .&. mask2

-- |
-- >>> :main
-- 2,0,7,3,0,3,1,3,7
-- 247839539763386
main :: IO ()
main = do
    (regs, prog) <- readParsed 2024 17 parser

    putStrLn . intercalate "," . map show $ run prog regs 0
    print . minimum $ findA prog
