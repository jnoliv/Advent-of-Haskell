{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, TupleSections #-}

import Advent.Megaparsec (readParsedLines, letterChar, sepBy, some, Parser, (<|>))
import Control.Lens ((^.), Field2(_2), Field3(_3))
import Data.Bifunctor (bimap, second)
import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromJust)

import Data.Map (Map, (!))
import Data.Map qualified as Map

type Pulses = [(String,String,Bool)] -- [(src, dst, val)]

type Modules = Map String Module
data Module  = Broadcaster [String] | FlipFlop Bool [String] | Conjunction (Map String Bool) [String] | Output [Bool]

destinations :: Module -> [String]
destinations (Broadcaster   dsts) = dsts
destinations (FlipFlop    _ dsts) = dsts
destinations (Conjunction _ dsts) = dsts

inputs :: Module -> [(String,Bool)]
inputs (Conjunction ins _) = Map.toAscList ins

parser :: Parser (String, Module)
parser = (second (FlipFlop        False) <$> ("%" *> parserModule))
     <|> (second (Conjunction Map.empty) <$> ("&" *> parserModule))
     <|> (second  Broadcaster            <$>         parserModule)
    where
        parserModule = (,) <$> some letterChar              <* " -> "
                           <*> some letterChar `sepBy` ", "

setup :: Modules -> Modules
setup modules = foldr updateMap modules (Map.assocs modules)
    where
        updateMap (src,mod) modules = foldr (Map.update (Just . addInput src)) modules (destinations mod)

        addInput src (Conjunction ins dsts) = Conjunction (Map.insert src False ins) dsts
        addInput   _                   mod  = mod

process :: Int -> [Int] -> [String] -> Modules -> Pulses -> ([Int],[String],Modules)
process    _        counts triggered modules                         [] = (counts,triggered,modules)
process iter [lows, highs] triggered modules ((src, dst, val) : pulses) =
    if dst `Map.member` modules
        then uncurry (process iter newCount triggered') . bimap setMod (pulses ++) $ processModule (modules Map.! dst) val
        else process iter newCount triggered' modules pulses
    where
        triggered' = if dst == "ql" && val then src : triggered else triggered

        newCount = bool [lows + 1, highs] [lows, highs + 1] val
        setMod m = Map.insert dst m modules

        processModule :: Module -> Bool -> (Module,Pulses)
        processModule (Output          outs)   val = (Output   (val : outs),                     [])
        processModule (Broadcaster     dsts)   val = (Broadcaster      dsts, map (dst,,val)    dsts)
        processModule (FlipFlop state  dsts)  True = (FlipFlop state   dsts,                     [])
        processModule (FlipFlop state  dsts) False = (FlipFlop state'  dsts, map (dst,,state') dsts)
            where state' = not state
        processModule (Conjunction ins dsts)   val = (Conjunction ins' dsts, map (dst,,out)    dsts)
            where
                ins' = Map.insert src val ins
                out  = not (and ins')

press :: (Int, [Int], Map String Int, Modules) -> (Int, [Int], Map String Int, Modules)
press (iter, count, triggered, modules) = (iter + 1, count', triggered'', modules')
    where
        (count', triggered', modules') = process iter count [] (resetOutput "rx" modules) [("button", "broadcaster", False)]

        triggered'' = foldr (\k m -> Map.insertWith min k iter m) triggered triggered'

resetOutput :: String -> Modules -> Modules
resetOutput = Map.update (Just . reset)
    where
        reset (Output outs) = Output []

-- |
-- >>> :main
-- 787056720
-- 212986464842911
main :: IO ()
main = do
    input <- Map.fromList <$> readParsedLines 2023 20 parser

    let modules       = Map.insert "rx" (Output []) (setup input)
        buttonPresses = iterate press (1, [0,0], Map.empty, modules)

        pulseCounts = (buttonPresses !! 1000) ^._2
        loopSizes   = (^._3) . fromJust $ find ((== 4) . Map.size . (^._3)) buttonPresses

    print (product pulseCounts)
    print $ foldr1 lcm loopSizes
