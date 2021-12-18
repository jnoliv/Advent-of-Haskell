{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (readParsedLines, decimal, Parser, (<|>), try)
import Advent.Utils (combinations)

-- pair of snailfish numbers or regular number
data Number = P Number Number | R Int

instance Show Number where
    show (R n)     = show n
    show (P n1 n2) = "[" ++ show n1 ++ "," ++ show n2 ++"]" 

data Op = Reduced
        | None
        | Exploded Int Int
        | ExplodedL Int
        | ExplodedR Int
    deriving (Eq, Show)

format :: Parser Number
format = number
    where
        number = try (P <$> ("[" *> number) <* "," <*> number <* "]")
                 <|> (R <$> decimal)

-- |
-- >> :main
-- 3051
-- 4812
main :: IO ()
main = do
    numbers <- readParsedLines 2021 18 format

    let sumAll = foldl1 add numbers

        pairsO = combinations 2 numbers
        pairs  = pairsO ++ map reverse pairsO

        pairSums = map (\[a,b] -> add a b) pairs


    print $ magnitude sumAll
    print . maximum $ map magnitude pairSums

-- |
-- >>> add (P (P (P (P (R 4) (R 3)) (R 4)) (R 4)) (P (R 7) (P (P (R 8) (R 4)) (R 9)))) (P (R 1) (R 1))
-- P (P (P (P (R 0) (R 7)) (R 4)) (P (P (R 7) (R 8)) (P (R 6) (R 0)))) (P (R 8) (R 1))
add :: Number -> Number -> Number
add a b = reduce (P a b, Reduced)

-- |
-- >>> reduce 0 (P (P (P (P (P (R 9) (R 8)) (R 1)) (R 2)) (R 3)) (R 4)) None
-- (P (P (P (P (R 0) (R 9)) (R 2)) (R 3)) (R 4),ExplodedL 9)
--
-- >>> reduce 0 (P (R 7) (P (R 6) (P (R 5) (P (R 4) (P (R 3) (R 2)))))) None
-- (P (R 7) (P (R 6) (P (R 5) (P (R 7) (R 0)))),ExplodedR 2)
--
-- >>> reduce 0 (P (P (R 3) (P (R 2) (P (R 1) (P (R 7) (R 3))))) (P (R 6) (P (R 5) (P (R 4) (P (R 3) (R 2)))))) None 
-- (P (P (R 3) (P (R 2) (P (R 8) (R 0)))) (P (R 9) (P (R 5) (P (R 4) (P (R 3) (R 2))))),Reduced)
--
-- >>> reduce 0 (P (P (P (P (R 0) (R 7)) (R 4)) (P (R 15) (P (R 0) (R 13)))) (P (R 1) (R 1))) None
-- (P (P (P (P (R 0) (R 7)) (R 4)) (P (P (R 7) (R 8)) (P (R 0) (R 13)))) (P (R 1) (R 1)),Reduced)
--
-- >>> reduce 0 (P (P (P (P (R 0) (R 7)) (R 4)) (P (P (R 7) (R 8)) (P (R 0) (R 13)))) (P (R 1) (R 1))) None
-- (P (P (P (P (R 0) (R 7)) (R 4)) (P (P (R 7) (R 8)) (P (R 0) (P (R 6) (R 7))))) (P (R 1) (R 1)),Reduced)
reduce :: (Number, Op) -> Number
reduce (n, None) = n
reduce (n, _)
    | opE /= None = reduce (exploded, opE)
    | otherwise   = reduce $ split n None
    where
        (exploded, opE) = explode 0 n None

split :: Number -> Op -> (Number, Op)
split (P left right) None
    | opL == Reduced = (P leftS right, Reduced)
    | otherwise      = (P left rightS, opR)
    where
        (leftS,  opL) = split left  None
        (rightS, opR) = split right None
split (R n)  None
    | n >= 10   = (P (R d) (R $ d + m), Reduced)
    | otherwise = (R n,                 None)
    where
        (d,m) = divMod n 2

explode :: Int -> Number -> Op -> (Number, Op)
explode _       (R n1)            None           = (R n1, None) 
explode _       (R n1)            (ExplodedL n2) = (R $ n1 + n2, Reduced)
explode _       (R n1)            (ExplodedR n2) = (R $ n1 + n2, Reduced)
explode depth n@(P (R n1) (R n2)) None
    | depth >= 4 = (R 0, Exploded n1 n2)
    | otherwise  = (n, None)
explode depth   (P left right) (ExplodedL l) = (P left rightE, Reduced)
    where
        (rightE, Reduced) = explode (depth + 1) right (ExplodedL l)
explode depth   (P left right) (ExplodedR r) = (P leftE right, Reduced)
    where
        (leftE, Reduced) = explode (depth + 1) left (ExplodedR r)
explode depth n@(P left right) None
    | opL == Reduced                = (P leftR right, Reduced)
    | opL == None && opR == None    = (n, None)
    | opL == None && opR == Reduced = (P left rightR, Reduced)
    | Exploded l r <- opL           = explodeR r (ExplodedL l)
    | ExplodedL l  <- opL           = (P leftR right, ExplodedL l)
    | ExplodedR r  <- opL           = explodeR r Reduced
    | Exploded l r <- opR           = explodeL l (ExplodedR r)
    | ExplodedL l  <- opR           = explodeL l Reduced
    | ExplodedR r  <- opR           = (P left rightR, ExplodedR r)
    where
        (leftR,  opL) = explode (depth + 1) left  None
        (rightR, opR) = explode (depth + 1) right None

        explodeR r opRes = (P leftR rightE, opRes)
            where (rightE, Reduced) = explode (depth + 1) right (ExplodedR r)
        
        explodeL l opRes = (P leftE rightR, opRes)
            where (leftE, Reduced) = explode (depth + 1) left (ExplodedL l)

explode depth number op = error $ "Depth: " ++ show depth ++ "\nNumber: " ++ show number ++ "\nOp: " ++ show op

magnitude :: Number -> Int
magnitude (R n) = n
magnitude (P l r) = 3 * (magnitude l) + 2 * (magnitude r)
