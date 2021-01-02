import Advent.Megaparsec
import Advent.Utils (replace, readBin, showBin)
import Data.Bits ((.&.), (.|.))
import qualified Data.IntMap as M

type Mem = M.IntMap Int

data Inst = Mask  String    -- Mask Bitmask
          | Write Int Int   -- Write Address Value
    deriving (Show)

program :: Parser Inst
program = Mask <$> (string "mask = " *> many alphaNumChar)
     <|> Write <$> (string "mem[" *> decimal) <*> (string "] = " *> decimal)

-- | Convert the bitmask to a bitwise-and mask and a bitwise-or mask that
-- when applied in succession, result in the same as the described mask
toMasks :: String -> (Int,Int)
toMasks = (,) <$> conv '1' <*> conv '0'
    where conv def = readBin . replace 'X' def

-- | Execute the instructions, returning the memory after program execution 
execute :: [Inst] -> Mem -> (Int, Int) -> Mem
execute [] mem _ = mem
execute (i:is) mem mask =
    case i of
        Mask m              -> execute is mem $ toMasks m
        Write address value -> execute is (update mem mask address value) mask
    where
        update mem mask add val        = M.insert add (applyMask mask val) mem
        applyMask (andMask,orMask) val = val .&. andMask .|. orMask

-- | Execute the instructions per version 2, returning the memory after program execution 
executeV2 :: [Inst] -> Mem -> String -> Mem
executeV2 [] mem _ = mem
executeV2 (i:is) mem mask =
    case i of
        Mask m              -> executeV2 is mem m
        Write address value -> executeV2 is (update mem mask address value) mask
    where
        update mem mask add val = foldl (\m a -> M.insert a val m) mem $ applyMaskV2 mask add

-- | Apply the version 2 mask to the address
applyMaskV2 :: String -> Int -> [Int]
applyMaskV2 mask val = map readBin . applyMaskV2' mask $ paddedBinVal
    where binVal       = showBin val
          paddedBinVal = replicate (36 - length binVal) '0' ++ binVal
          
          applyMaskV2' [] _ = [""]
          applyMaskV2' _ [] = [""]
          applyMaskV2' (m:ms) (v:vs) =
              (:) <$> case m of
                          '0' -> return v
                          '1' -> return '1'
                          'X' -> ['0','1']
                  <*> applyMaskV2' ms vs

-- |
-- >>> :main
-- 11612740949946
-- 3394509207186
main :: IO()
main = do
    insts <- readParsedLines 2020 14 program

    let mem = execute insts M.empty (0,0)
        sum = M.foldl (+) 0 mem

        mem2 = executeV2 insts M.empty ""
        sum2 = M.foldl (+) 0 mem2

    print sum
    print sum2
