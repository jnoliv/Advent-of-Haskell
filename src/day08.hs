import qualified Common.AdventAPI as AdventAPI

import Data.Maybe (fromJust)
import qualified Data.Set    as Set
import qualified Data.Vector as Vector

type Op      = String
type Program = Vector.Vector (Op, Int)

parseOp :: String -> (Op, Int)
parseOp line = (op, arg)
    where [op,rArg] = words line
          arg       = if head rArg == '+' then read $ tail rArg else read rArg

executeUntilLoop :: Program -> Set.Set Int -> Int -> Int -> (Bool, Int)
executeUntilLoop program visited pc acc
    | pc `Set.member` visited     = (False, acc)
    | pc >= Vector.length program = (True, acc)
    | otherwise                   =
        case op of
            "nop" -> executeUntilLoop program newVisited (succ pc) acc
            "acc" -> executeUntilLoop program newVisited (succ pc) (acc + arg)
            "jmp" -> executeUntilLoop program newVisited (pc + arg) acc
        where (op,arg)   = program Vector.! pc
              newVisited = Set.insert pc visited

tryRestoreRun :: Program -> Int -> Maybe Int
tryRestoreRun program adr
    | adr >= Vector.length program        = Nothing
    | fst (program Vector.! adr) == "acc" = tryRestoreRun program (succ adr)
    | otherwise                           =
        if success then Just result else tryRestoreRun program (succ adr)
        where uncorrupt ("jmp",arg) = ("nop",arg)
              uncorrupt ("nop",arg) = ("jmp",arg)
              newProgram            = program Vector.// [(adr, uncorrupt $ program Vector.! adr)]
              (success, result)     = executeUntilLoop newProgram Set.empty 0 0

main :: IO()
main = do
    contents <- AdventAPI.readInput 8 "../session-cookie.txt" "../input"

    let program = Vector.fromList . map parseOp $ lines contents
        result1 = executeUntilLoop program Set.empty 0 0
        result2 = tryRestoreRun program 0

    print . snd $ result1
    print . fromJust $ result2
