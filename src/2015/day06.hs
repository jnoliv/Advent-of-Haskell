{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments #-}

import Advent.Megaparsec
import Data.Vector.Unboxed.Mutable qualified as VM
import Control.Monad (foldM, forM_)

type Coord   = (Int, Int)

data Cmd     = On | Off | Toggle
data RawInst = RawInst Cmd Coord Coord
data Inst    = Inst (Int -> Int) Coord Coord

format :: Parser RawInst
format = RawInst <$> cmd <* " " <*> coord <* " through " <*> coord
    where 
        cmd   = "toggle"   $> Toggle
            <|> "turn on"  $> On
            <|> "turn off" $> Off
        coord = (,) <$> decimal <* "," <*> decimal

toInst :: RawInst -> Inst
toInst (RawInst On     c1 c2) = Inst (\_ -> 1) c1 c2
toInst (RawInst Off    c1 c2) = Inst (\_ -> 0) c1 c2
toInst (RawInst Toggle c1 c2) = Inst (\n -> if n == 0 then 1 else 0) c1 c2

toInst2 :: RawInst -> Inst
toInst2 (RawInst On     c1 c2) = Inst (+ 1) c1 c2
toInst2 (RawInst Off    c1 c2) = Inst (max 0 . pred) c1 c2
toInst2 (RawInst Toggle c1 c2) = Inst (+ 2) c1 c2

toIndex :: Coord -> Int
toIndex (r,c) = 1000 * r + c

add :: VM.IOVector Int -> Int -> Int -> IO Int
add vector a ind = (+ a) <$> VM.read vector ind

setup :: (Coord -> Int) -> [Inst] -> VM.IOVector Int -> IO Int
setup toIndex insts lights = do
    forM_ insts $ \(Inst f (r1,c1) (r2,c2)) -> do
        forM_ [(r,c) | r <- [r1..r2], c <- [c1..c2]] $ \coords -> do
            let index = toIndex coords

            brightness <- VM.read lights index
            VM.write lights index (f brightness)
    
    foldM (add lights) 0 [0 .. VM.length lights - 1]

-- |
-- >>> :main
-- 569999
-- 17836115
main :: IO ()
main = do
    rawInsts <- readParsedLines 2015 6 format

    lights1 <- VM.replicate (1000 * 1000) 0
    lights2 <- VM.replicate (1000 * 1000) 0

    brightness  <- setup toIndex (map toInst  rawInsts) lights1
    brightness2 <- setup toIndex (map toInst2 rawInsts) lights2

    print brightness
    print brightness2
