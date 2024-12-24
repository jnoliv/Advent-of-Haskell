{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (Parser, (<|>), endBy, decimal, many, alphaNumChar, readParsed)
import Advent.Utils (readBin, showBin)
import Data.Bits (xor, (.&.), (.|.))
import Data.Char (intToDigit, digitToInt)
import Data.Composition ((.:))
import Data.List (intercalate, sort)

import Data.Map (Map, (!))
import Data.Map qualified as Map

--                           Wire1  Op     Wire2
data Wire = Value Int | Gate String String String

parser :: Parser (Map String Wire)
parser = Map.fromList .: (++)
            <$> initial    `endBy` "\n" <* "\n"
            <*> connection `endBy` "\n"
    where
        initial     = (,) <$> wire <* ": " <*> (Value <$> decimal)
        connection  = flip (,) <$> gate <* " -> " <*> wire
        wire        = many alphaNumChar
        gate        = Gate <$> wire <* " " <*> op <* " " <*> wire
        op          = "AND" <|> "OR" <|> "XOR"

simulate :: Map String Wire -> Int
simulate = output . head . dropWhile (any isGate) . iterate step
    where
        isGate (Value    _) = False
        isGate (Gate _ _ _) = True

step :: Map String Wire -> Map String Wire
step wires = Map.map applyGate wires
    where
        applyGate v@(Value n)       = v
        applyGate g@(Gate w1 op w2) = case (wires ! w1, wires ! w2) of
            (Value n1, Value n2) -> Value $ applyOp n1 op n2
            _                    -> g

        applyOp w1 "AND" w2 = w1  .&. w2
        applyOp w1  "OR" w2 = w1  .|. w2
        applyOp w1 "XOR" w2 = w1 `xor` w2

output :: Map String Wire -> Int
output = readBin . map (toDigit . snd) . Map.toDescList .
         Map.filterWithKey (\k v -> head k == 'z')
    where
        toDigit (Value n) = intToDigit n

add :: Map String Wire -> Int -> Int -> Int
add wires x y = simulate . foldl (\m (k,v) -> Map.insert k v m) wires $ toWires 'x' x ++ toWires 'y' y
    where
        toWires c n = [(name c i, Value $ digitToInt d) | (i,d) <- zip [0..] . reverse . pad $ showBin n]
        pad s       = replicate (45 - length s) '0' ++ s
        name    c i
            | i < 10    = c : '0' : show i
            | otherwise = c :       show i

-- |
-- >>> :main
-- 38869984335432
-- drg,gvw,jbp,jgc,qjb,z15,z22,z35
main :: IO ()
main = do
    wires <- readParsed 2024 24 parser

    print $ simulate wires

{-
    I solved part 2 with a mix of coding and by hand:
    1. My first thought was that trying to add 0 and a number with a single bit on would tell me
       where the issues were. I did this for all bits, which showed issues in bits 8, 15, 22 and 35.
    2. Then, I looked at what the tree for a working bit looked like. For example, bit 2:

            dkg XOR ghb -> z02
            y02 XOR x02 -> dkg
            nqm  OR whj -> ghb
                y01 AND x01 -> nqm
                wwc AND nvv -> whj
                    x01 XOR y01 -> wwc
                    y00 AND x00 -> nvv

            x02 AND y02 -> gws

       As expected, there's an XOR between x02, y02 and the carry from the previous bits.
    3. Armed with this knowledge, I started looking at the input and building the tree for the bits
       with issues by hand. This soon revealed where the issues were, which I confirmed by swapping
       the wires and rerunning the tests mentioned in step 1. After doing this four times, all that
       was left was sorting the wire names and intercalating them with a comma.
    
    The following code is what I used for the tests mentioned in steps 1 and 3. Initially wires' was
    simply wires, but as I found issues I began swapping the wires to fix them.

    let swap w1 w2 wires
            | w1 `Map.notMember` wires = error $ "wrong wire name " ++ w1
            | w2 `Map.notMember` wires = error $ "wrong wire name " ++ w2
            | otherwise = Map.insert w2 tmp $ Map.insert w1 (wires ! w2) wires
            where
                tmp = wires ! w1

        wires' = swap "jbp" "z35" $ swap "drg" "z22" $ swap "gvw" "qjb" $ swap "z15" "jgc" wires

        tests = map (\n -> (n, 2^n, add wires' (2^n) 0)) [0..44]
        bad   = filter (uncurry (/=) . (\(_,a,b) -> (a,b)))

    mapM_ print $ bad tests
-}

    print . intercalate "," $ sort ["jbp","z35","drg","z22","gvw","qjb","z15","jgc"]
