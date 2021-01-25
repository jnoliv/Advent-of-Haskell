{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Data.Bifunctor (first, second)
import Data.Map (Map)
import Data.Map qualified as Map

type Reg   = Char
data Instr = Half Reg | Triple Reg | Increment Reg | Jump Int | JumpIfEven Reg Int | JumpIfOne Reg Int

format :: Parser Instr
format = (Half       <$> ("hlf " *> letterChar))
     <|> (Triple     <$> ("tpl " *> letterChar))
     <|> (Increment  <$> ("inc " *> letterChar))
     <|> (Jump       <$> ("jmp " *> sdecimal))
     <|> (JumpIfEven <$> ("jie " *> letterChar <* ", ") <*> sdecimal)
     <|> (JumpIfOne  <$> ("jio " *> letterChar <* ", ") <*> sdecimal)

execute :: [Instr] -> Map Reg Int -> Int
execute instr registers = go registers 0
    where
        size  = length instr

        go :: Map Reg Int -> Int -> Int
        go registers ip
            | ip >= size = read 'b'
            | otherwise  = case instr !! ip of
                Half       r   -> go (set r . (`div` 2) $ read r) (ip + 1)
                Triple     r   -> go (set r . (* 3)     $ read r) (ip + 1)
                Increment  r   -> go (set r . (+ 1)     $ read r) (ip + 1)
                Jump         o -> go registers                    (ip + o)
                JumpIfEven r o -> go registers                    (if read r `rem` 2 == 0 then ip + o else ip + 1)
                JumpIfOne  r o -> go registers                    (if read r         == 1 then ip + o else ip + 1)
            where
                read r   = Map.findWithDefault 0 r registers
                set  r v = Map.insert r v registers

-- |
-- >>> :main
-- 170
-- 247
main :: IO ()
main = do
    program <- readParsedLines 2015 23 format

    print $ execute program Map.empty
    print $ execute program (Map.singleton 'a' 1)
