{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Utils
import Data.Bifunctor
import Data.Char

import Debug.Trace

data Packet =
    Lit Int Int Int     | -- Lit version type value
    Op Int Int [Packet]   -- Op  version type sub-packets
    deriving Show

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"

-- |
-- >>> :main
-- 1007
-- 834151779165
main :: IO ()
main = do
    hexa <- init <$> readInputDefaults 2021 16

    let bin         = concatMap hexToBin hexa
        (packet, _) = readPacket bin

    print $ sumVersions packet
    print $ calculate   packet

-- |
-- >>> readPacket "110100101111111000101000"
-- (Lit 6 4 2021,"000")
--
-- >>> readPacket "00111000000000000110111101000101001010010001001000000000"
-- (Op 1 6 [Lit 6 4 10,Lit 2 4 20],"0000000")
--
-- >>> readPacket "11101110000000001101010000001100100000100011000001100000"
-- (Op 7 3 [Lit 2 4 1,Lit 4 4 2,Lit 1 4 3],"00000")
readPacket :: String -> (Packet, String)
readPacket stream
    | typeID == 4 = (Lit version typeID lit,     stream3a)
    | otherwise   = (Op  version typeID packets, stream5)
    where
        (version, stream1) = readVersion  stream
        (typeID,  stream2) = readType     stream1

        -- If literal packet this will be used
        (lit, stream3a) = readLit stream2
        
        -- Otherwise, read length type ID
        (lengthID : stream3b) = stream2

        (packets, stream5) = case lengthID of
            '0' -> let (len, stream4) = readLength stream3b in second (const (drop len stream4)) $ readPackets (take len stream4)
            '1' -> let (n,   stream4) = readAmount stream3b in readNPackets n stream4

readVersion, readType, readLength, readAmount :: String -> (Int, String)
readVersion = first readBin . splitAt 3
readType    = first readBin . splitAt 3
readLength  = first readBin . splitAt 15
readAmount  = first readBin . splitAt 11

readLit :: String -> (Int, String)
readLit = first readBin . f
    where
        f ('0':a:b:c:d:stream) = ([a,b,c,d], stream)
        f ('1':a:b:c:d:stream) = first ([a,b,c,d] ++) $ f stream

readPackets :: String -> ([Packet], String)
readPackets = first reverse . f []
    where
        f packets []     = (packets, [])
        f packets stream = uncurry f . first (: packets) $ readPacket stream

readNPackets :: Int -> String -> ([Packet], String)
readNPackets n = first reverse . f n []
    where
        f 0 packets stream = (packets, stream)
        f n packets stream = uncurry (f (n - 1)) . first (: packets) $ readPacket stream

sumVersions :: Packet -> Int
sumVersions (Lit v _ _)  = v
sumVersions (Op  v _ ps) = (+ v) . sum $ map sumVersions ps

calculate :: Packet -> Int
calculate (Lit _ _ v)       = v
calculate (Op  _ 0 ps)      = sum     $ map calculate ps
calculate (Op  _ 1 ps)      = product $ map calculate ps
calculate (Op  _ 2 ps)      = minimum $ map calculate ps
calculate (Op  _ 3 ps)      = maximum $ map calculate ps
calculate (Op  _ 5 [p1,p2]) = if calculate p1 >  calculate p2 then 1 else 0
calculate (Op  _ 6 [p1,p2]) = if calculate p1 <  calculate p2 then 1 else 0
calculate (Op  _ 7 [p1,p2]) = if calculate p1 == calculate p2 then 1 else 0
