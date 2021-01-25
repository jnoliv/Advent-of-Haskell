{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec
import Data.Maybe (fromJust)

data Boss   = Boss   Int Int        -- HP ATK
    deriving Show
data Player = Player Int Int Int    -- HP MANA DEF
    deriving Show

data Spell  = Missile | Drain | Shield | Poison | Recharge
    deriving (Eq, Show)
    
data Effect = Effect Spell Int
    deriving Show

instance Eq Effect where
    (Effect sp _) == (Effect sp' _) = sp == sp'

type GameSt = ([Effect], Player, Boss)

format :: Parser Boss
format = Boss <$> ("Hit Points: " *> decimal)
              <*> ("\nDamage: "   *> decimal)

optimalBattle :: Bool -> Player -> Boss -> Int
optimalBattle hardMode player boss = fromJust $ go True 0 ([], player, boss)
    where
        go pTurn usedMana gameSt
            | death pStats   = Nothing
            | victory bStats = Just usedMana
            | not pTurn      = go True usedMana $ (effs, bossAttack pStats bStats, bStats)
            | otherwise      = maybeMinimum . filter (/= Nothing) $ map castAndGo castable
            where
                gameSt'@(effs, pStats, bStats) = applyMode $ applyEffects gameSt
                death (Player hp _ _)          = hp <= 0
                victory (Boss hp _)            = hp <= 0
                castable                       = filter (canCast effs pStats) [Missile, Drain, Shield, Poison, Recharge]
                castAndGo spell                = go False (usedMana + manaCost spell) $ cast spell gameSt'
                maybeMinimum l                 = if null l then Nothing else minimum l
                bossAttack (Player hp mana def) (Boss _ atk)     = Player (hp - max 1 (atk - def)) mana def
                applyMode gSt@(effs, Player hp mana def, bStats) = if pTurn && hardMode then (effs, Player (hp - 1) mana def, bStats) else gSt

-- | Apply active effects and remove any that fizzle
-- >>> applyEffects ([], Player 50 500 0, Boss 71 10)
-- ([],Player 50 500 0,Boss 71 10)
--
-- >>> applyEffects ([Effect Poison 3, Effect Recharge 2], Player 50 500 0, Boss 71 10)
-- ([Effect Poison 2,Effect Recharge 1],Player 50 601 0,Boss 68 10)
--
-- >>> applyEffects ([Effect Poison 1, Effect Recharge 1, Effect Shield 1], Player 50 500 7, Boss 71 10)
-- ([],Player 50 601 0,Boss 68 10)
applyEffects :: GameSt -> GameSt
applyEffects (effs, pStats, bStats) = foldr applyEffect ([], pStats, bStats) effs
    where
        tick (Effect sp 1) effs         = effs
        tick (Effect sp n) effs         = Effect sp (n - 1) : effs

        applyEffect     (Effect Shield 1)   (effs, pStats, bStats) = (effs, setDef 0 pStats, bStats)
        applyEffect eff@(Effect Shield dur) (effs, pStats, bStats) = (tick eff effs, pStats, bStats)
        applyEffect eff@(Effect Poison   n) (effs, pStats, bStats) = (tick eff effs, pStats, dmg 3 bStats)
        applyEffect eff@(Effect Recharge n) (effs, pStats, bStats) = (tick eff effs, recharge 101 pStats, bStats)  

-- | Can a spell be cast given the current active effects and the player's available mana
-- >>> and $ map (canCast [] (Player 50 500 0)) [Missile, Drain, Shield, Poison, Recharge]
-- True
--
-- >>> map (canCast [Effect Shield 3] (Player 50 200 7)) [Missile, Drain, Shield, Poison, Recharge]
-- [True,True,False,True,False]
canCast :: [Effect] -> Player -> Spell -> Bool
canCast effs (Player _ mana _) spell = (Effect spell 0) `notElem` effs && mana >= manaCost spell

-- | Cast the given spell
-- >>> cast Missile ([], Player 50 500 0, Boss 71 10)
-- ([],Player 50 447 0,Boss 67 10)
--
-- >>> cast Drain ([], Player 50 500 0, Boss 71 10)
-- ([],Player 52 427 0,Boss 69 10)
--
-- >>> cast Shield ([], Player 50 500 0, Boss 71 10)
-- ([Effect Shield 6],Player 50 387 7,Boss 71 10)
--
-- >>> cast Poison ([], Player 50 500 0, Boss 71 10)
-- ([Effect Poison 6],Player 50 327 0,Boss 71 10)
--
-- >>> cast Recharge ([], Player 50 500 0, Boss 71 10)
-- ([Effect Recharge 5],Player 50 271 0,Boss 71 10)
--
cast :: Spell -> GameSt -> GameSt
cast Missile  (effs, pStats, bStats) = (effs, discharge Missile pStats, dmg 4 bStats)
cast Drain    (effs, pStats, bStats) = (effs, heal 2 $ discharge Drain pStats, dmg 2 bStats)
cast Shield   (effs, pStats, bStats) = (Effect Shield   6 : effs, setDef 7 $ discharge Shield pStats, bStats)
cast Poison   (effs, pStats, bStats) = (Effect Poison   6 : effs, discharge Poison pStats, bStats)
cast Recharge (effs, pStats, bStats) = (Effect Recharge 5 : effs, discharge Recharge pStats, bStats)

setDef :: Int -> Player -> Player
setDef n (Player hp mana def) = Player hp mana n

recharge :: Int -> Player -> Player
recharge n (Player hp mana def) = Player hp (mana + n) def

discharge :: Spell -> Player -> Player
discharge spell (Player hp mana def) = Player hp (mana - (manaCost spell)) def

heal :: Int -> Player -> Player
heal n (Player hp mana def) = Player (hp + n) mana def

dmg :: Int -> Boss -> Boss
dmg n (Boss hp atk) = Boss (hp - n) atk

manaCost :: Spell -> Int
manaCost Missile  = 53
manaCost Drain    = 73
manaCost Shield   = 113
manaCost Poison   = 173
manaCost Recharge = 229

-- |
-- >>> :main
-- 1824
-- 1937
main :: IO ()
main = do
    bossStats <- readParsed 2015 22 format

    let playerStats = Player 50 500 0

    print $ optimalBattle False playerStats bossStats
    print $ optimalBattle True playerStats bossStats
