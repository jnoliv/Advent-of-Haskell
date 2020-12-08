import qualified Common.AdventAPI as AdventAPI

import qualified Data.Set    as Set
import qualified Data.Vector as Vector

type Op = String

parseOp :: String -> (Op, Int)
parseOp line = (op, arg)
    where [op,rArg] = words line
          arg       = if head rArg == '+' then read $ tail rArg else read rArg

executeUntilLoop :: Vector.Vector (Op, Int) -> Set.Set Int -> Int -> Int -> Int
executeUntilLoop program visited pc acc =
    if pc `Set.member` visited
        then acc
        else let (op,arg)   = program Vector.! pc
                 newVisited = Set.insert pc visited
             in case op of
                 "nop" -> executeUntilLoop program newVisited (succ pc) acc
                 "acc" -> executeUntilLoop program newVisited (succ pc) (acc + arg)
                 "jmp" -> executeUntilLoop program newVisited (pc + arg) acc


main :: IO()
main = do
    contents <- AdventAPI.readInput 8 "../session-cookie.txt" "../input"

    let program = Vector.fromList . map parseOp $ lines contents
        result  = executeUntilLoop program Set.empty 0 0

    print result
