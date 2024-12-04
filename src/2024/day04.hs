import Advent.API (readInputDefaults)
import Advent.Utils (count)
import Data.List (transpose)

diagonals :: [[a]] -> [[a]]
diagonals = (++) <$> transpose . zipWith drop [0..]
                 <*> transpose . map reverse . tail . zipWith take [0..]

allDirections :: [[a]] -> [[a]]
allDirections = ((++) <$> id <*> map reverse) . concat . sequence [id, transpose, diagonals, diagonals . map reverse]

countXMAS :: String -> Int
countXMAS ('X':'M':'A':'S':r) = 1 + countXMAS r
countXMAS []                  = 0
countXMAS (_:r)               = countXMAS r

subMatrices :: Int -> [[a]] -> [[[a]]]
subMatrices n = map transpose . concatMap (groups n . transpose) . groups n
    where
        groups n l = [take n . drop x $ l | x <- [0 .. length l - n]]

isCrossMAS :: [String] -> Bool
isCrossMAS = any crossMAS . sequence [id, transpose, reverse, reverse . transpose]
    where
        crossMAS [['M', _ ,'M'],
                  [ _ ,'A', _ ],
                  ['S', _ ,'S']] = True
        crossMAS _               = False

-- |
-- >>> :main
-- 2524
-- 1873
main :: IO ()
main = do
    input <- lines <$> readInputDefaults 2024 4

    print . sum . map countXMAS . allDirections $ input
    print . count isCrossMAS    . subMatrices 3 $ input
