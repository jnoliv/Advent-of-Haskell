{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec

data Ingredient = Ing { name :: String,
                        cap :: Int,
                        dur :: Int,
                        flv :: Int,
                        tex :: Int,
                        cal :: Int }

format :: Parser Ingredient
format = Ing
     <$> some letterChar
     <*> (": capacity "   *> sdecimal)
     <*> (", durability " *> sdecimal)
     <*> (", flavor "     *> sdecimal)
     <*> (", texture "    *> sdecimal)
     <*> (", calories "   *> sdecimal)

score :: [Ingredient] -> [Int] -> Int
score ings ns = product . map (\f -> max 0 . sum . zipWith (*) ns $ map f ings) $ [cap, dur, flv, tex]

totalCalories :: [Ingredient] -> [Int] -> Int
totalCalories ings ns = sum . zipWith (*) ns $ map cal ings

optimize :: Bool -> [Ingredient] -> Int -> Int
optimize limitCalories ings = maximum . map (score ings) . filterPerCalories limitCalories . recipes (length ings)
    where
        recipes 1     limit = [[limit]]
        recipes nIngs limit = do
            n <- [0 .. limit]
            ns <- recipes (nIngs - 1) (limit - n)
            return (n : ns)
        
        filterPerCalories False = id
        filterPerCalories True  = filter ((== 500) . totalCalories ings)

-- |
-- >>> :main
-- 21367368
-- 1766400
main :: IO ()
main = do
    ings <- readParsedLines 2015 15 format

    print $ optimize False ings 100
    print $ optimize True  ings 100
