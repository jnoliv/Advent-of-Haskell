import Advent.API (readInputDefaults)
import Text.Regex.PCRE ((=~))
import Control.Monad (forM_)

doMul :: [String] -> Int
doMul [_,a,b] = (read a :: Int) * (read b :: Int)

filterMuls :: [[String]] -> [[String]]
filterMuls = f True
    where
        f     _ []                       = []
        f     _ (["do()",_,_]    : muls) =       f True muls
        f     _ (["don't()",_,_] : muls) =       f False muls
        f  True (mul             : muls) = mul : f True muls
        f False (mul             : muls) =       f False muls

-- |
-- >>> :main
-- 174960292
-- 56275602
main :: IO ()
main = do
    input <- readInputDefaults 2024 3

    let muls  = input =~ "mul\\((\\d{1,3}),(\\d{1,3})\\)" :: [[String]]
        muls2 = input =~ "mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)" :: [[String]]

    print . sum . map doMul              $ muls
    print . sum . map doMul . filterMuls $ muls2
