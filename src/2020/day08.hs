import AdventAPI
import Data.Maybe (maybe)
import qualified Data.IntSet    as Set
import qualified Data.Vector as Vector

type Op      = String
type Program = Vector.Vector (Op, Int)

-- | Parse an input line into a (operation, argument) pair
parseOp :: String -> (Op, Int)
parseOp line = (op, arg)
    where [op,rArg] = words line
          arg       = if head rArg == '+' then read $ tail rArg else read rArg

-- | Execute the program until either it succeeds or a loop is detected
executeUntilLoop :: Program -> Set.IntSet -> Int -> Int -> (Bool, Int)
executeUntilLoop program visited pc acc
    | pc `Set.member` visited     = (False, acc)
    | pc >= Vector.length program = (True,  acc)
    | otherwise                   =
        case op of
            "nop" -> executeUntilLoop program visited' (succ pc)  acc
            "acc" -> executeUntilLoop program visited' (succ pc) (acc + arg)
            "jmp" -> executeUntilLoop program visited' (pc + arg) acc
        where (op,arg)   = program Vector.! pc
              visited' = Set.insert pc visited

-- | Brute force changing nops to jmps (and vice-versa) until the program terminates
tryRestoreRun :: Program -> Int -> Maybe Int
tryRestoreRun program adr
    | adr >= Vector.length program        = Nothing
    | fst (program Vector.! adr) == "acc" = tryRestoreRun program (succ adr)
    | otherwise                           =
        if success then Just result else tryRestoreRun program (succ adr)
        where uncorrupt ("jmp",arg) = ("nop",arg)
              uncorrupt ("nop",arg) = ("jmp",arg)
              program'              = program Vector.// [(adr, uncorrupt $ program Vector.! adr)]
              (success, result)     = executeUntilLoop program' Set.empty 0 0

main :: IO()
main = do
    contents <- readInputDefaults 2020 8

    let program = Vector.fromList . map parseOp $ lines contents
        result1 = executeUntilLoop program Set.empty 0 0
        result2 = tryRestoreRun program 0

    print . snd $ result1
    putStrLn . maybe "could not restore program" show $ result2
