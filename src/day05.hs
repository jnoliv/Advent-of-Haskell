import qualified Common.AdventAPI as AdventAPI

bspToBit :: Char -> Int
bspToBit c
    | c == 'F' || c == 'L' = 0
    | c == 'B' || c == 'R' = 1

binToDec :: [Int] -> Int
binToDec = foldl (\acc bit -> 2*acc + bit) 0

main :: IO()
main = do
    contents <- AdventAPI.readInput 5 "../session-cookie.txt" "../input"

    print . maximum . map (binToDec . map bspToBit) . lines $ contents
