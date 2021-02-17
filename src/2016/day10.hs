{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

data Recv  = Bot Int | Out Int
data Instr = InitSt Int Int | BotInstr Int Recv Recv

format :: Parser Instr
format = (flip InitSt <$> ("value " *> decimal <* " goes to bot ")
                      <*> decimal)
     <|> (BotInstr    <$> ("bot " *> decimal <* " gives low to ")
                      <*> recv <* " and high to "
                      <*> recv)
    where
        recv = (Bot <$> ("bot "    *> decimal))
           <|> (Out <$> ("output " *> decimal))

give :: [Int] -> [Int] -> [Int]
give a b = sort $ a ++ b

simulate :: Map Int (Recv,Recv) -> Map Int [Int] -> (Map Int [Int], Map Int Int)
simulate instrs initSt = go initSt Map.empty Map.empty
    where
        applyInstr b [vl,vh] (bots, output) = giveVal vl l $ giveVal vh h (bots, output)
            where
                (l,h) = fromJust $ Map.lookup b instrs
                giveVal v (Bot b) (bots, output) = (Map.insertWith give b [v] bots, output)
                giveVal v (Out o) (bots, output) = (bots, Map.insert o v output)

        go bots filledBots output
            | Map.null bots = (filledBots, output)
            | otherwise     = go bots'' filledBots'' output'
            where
                (bots', filledBots') = Map.partition ((< 2) . length) bots
                (bots'', output')    = Map.foldrWithKey applyInstr (bots', output) filledBots'
                filledBots''         = Map.union filledBots filledBots'

-- |
-- >>> :main
-- 93
-- 47101
main :: IO ()
main = do
    input <- readParsedLines 2016 10 format

    let instrs = Map.fromList [(k,(l,h)) | BotInstr k l h <- input]
        initSt = Map.fromListWith give [(k,[v]) | InitSt k v <- input]

        (bots, out) = simulate instrs initSt
        botNumber   = fst . head . filter ((== [17,61]) . snd) $ Map.toList bots
        outProd     = product . take 3 . map snd $ Map.toList out

    print botNumber
    print outProd

