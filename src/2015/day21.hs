{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec
import Advent.Utils (combinations)
import Data.Bifunctor (second)

data Stats = Stats Int Int Int -- HP ATK DEF

format :: Parser Stats
format = Stats <$> ("Hit Points: " *> decimal)
               <*> ("\nDamage: "   *> decimal)
               <*> ("\nArmor: "    *> decimal)

weapons, armour, rings :: [(Int, (Stats -> Stats))]
weapons = map (second atkPlus) [(8,4), (10,5), (25,6), (40,7), (74,8)]
armour  = map (second defPlus) [(13,1), (31,2), (53,3), (75,4), (102,5)]
rings   = map (second atkPlus) [(25,1), (50,2), (100,3)]
       ++ map (second defPlus) [(20,1), (40,2), (80,3)]

atkPlus, defPlus :: Int -> Stats -> Stats
atkPlus n (Stats hp atk def) = Stats hp (atk + n) def
defPlus n (Stats hp atk def) = Stats hp atk (def + n)

equipmentCombinations :: [(Int, (Stats -> Stats))]
equipmentCombinations = do
    let combinationsRange l = concatMap (\n -> combinations n l)
        f (c,s) (c',s')     = (c + c', s . s')
    
    w <- combinationsRange weapons [1]
    a <- combinationsRange armour  [0,1]
    r <- combinationsRange rings   [0,1,2]

    return . foldr f (0,id) $ w ++ a ++ r

-- | Returns true if first character wins, false otherwise
-- >>> battle (Stats 8 5 5) (Stats 12 7 2)
-- True
--
-- >>> battle (Stats 8 5 5) (Stats 13 7 2)
-- False
battle :: Stats -> Stats -> Bool
battle (Stats hp1 atk1 def1) (Stats hp2 atk2 def2) = ttk hp2 dmg1 <= ttk hp1 dmg2
    where 
        dmg1       = max 1 (atk1 - def2)
        dmg2       = max 1 (atk2 - def1)
        ttk hp dmg   -- Time To Kill
            | r == 0    = q
            | otherwise = q + 1
            where (q, r) = quotRem hp dmg

allBattles :: Stats -> Stats -> [(Int, Bool)]
allBattles pStats bStats = map (second equipBattle) equipmentCombinations
    where
        equipBattle = (`battle` bStats) .  ($ pStats)

cheapestWin, priciestLoss :: Stats -> Stats -> Int
cheapestWin  pStats bStats = minimum . map fst . filter snd         $ allBattles pStats bStats
priciestLoss pStats bStats = maximum . map fst . filter (not . snd) $ allBattles pStats bStats

-- |
-- >>> :main
-- 78
-- 148
main :: IO ()
main = do
    bossStats <- readParsed 2015 21 format

    let playerStats = Stats 100 0 0

    print $ cheapestWin  playerStats bossStats
    print $ priciestLoss playerStats bossStats
