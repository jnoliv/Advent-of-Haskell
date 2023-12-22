{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections #-}

import Advent.Megaparsec (decimal, readParsedLines, Parser)
import Advent.Coord.Grid3 ((.-), (.+), getZ, Point)
import Advent.Utils (takeUntil)

import Data.Bifunctor (second)
import Data.List (nub)
import Data.Maybe (mapMaybe)

import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type Edges  = (Point,Point)
type Brick  = [Point]
type Bricks = Map Int Brick
type Stack  = Map Point Int

parser :: Parser Edges
parser = (,) <$> point <* "~" <*> point
    where
        point = (,,) <$> decimal <* ","
                     <*> decimal <* ","
                     <*> decimal

voxels :: Edges -> Brick
voxels (s,e) = takeUntil (/= e) (iterate (.+ d) s)
    where
        d = map3 (min 1) (e .- s)

        map3 f (a,b,c) = (f a, f b, f c)

height :: Stack -> Int
height = Map.foldrWithKey (\p _ n -> max n (getZ p)) 0

toBricks :: Stack -> Bricks
toBricks = foldr (\(v,k) m -> Map.insertWith (++) k [v] m) Map.empty . Map.assocs

moveDown :: Brick -> Brick
moveDown = map (.- (0,0,1))

canFall :: Stack -> (Int,Brick) -> Bool
canFall stack (id, brick) = all (\p -> getZ p >= 1 && (p `Map.notMember` stack || stack ! p == id)) (moveDown brick)

fall :: Stack -> Stack
fall stack = foldl f stack planes
    where
        planes = [2 .. height stack]

        f :: Stack -> Int -> Stack
        f stack z = foldr fallBrick stack planeBricks
            where
                bricks = toBricks stack

                planeVoxels = Map.filterWithKey (\p _ -> getZ p == z) stack
                planeBricks = map (\k -> (k, bricks ! k)) . nub . map snd $ Map.toList planeVoxels

        fallBrick :: (Int, Brick) -> Stack -> Stack
        fallBrick (id,brick) stack = foldr (`Map.insert` id) (foldr Map.delete stack brick) stable
            where
                (_,stable) = until (not . canFall stack) (second moveDown) (id, brick)

support :: (Brick -> Brick) -> Stack -> [(Int, [Int])]
support move stack = map f . Map.toList $ toBricks stack
    where
        f :: (Int,Brick) -> (Int, [Int])
        f (id, brick) = (id, ids)
            where
                ids = nub . filter (/= id) . mapMaybe (`Map.lookup` stack) $ move brick

chainReactions :: Stack -> [Int] -> Int
chainReactions stack starts = sum removedCounts - length starts
    where
        removedCounts = map (f supportedBy . (: [])) starts

        onGround    = Map.filter (any ((== 1) . getZ)) $ toBricks stack
        notOnGround = (`Map.notMember` onGround) . fst

        supportedBy = Map.fromList . map (second Set.fromList) . filter notOnGround $ support moveDown stack

        f :: Map Int (Set Int) -> [Int] -> Int
        f           _   [] = 0
        f supportedBy curs = length curs + f afterRemoving unsupported
            where
                deleteAll     = flip (foldr Set.delete) curs
                afterRemoving = foldr Map.delete (Map.map deleteAll supportedBy) curs
                unsupported   = Map.keys $ Map.filter null afterRemoving

-- |
-- >>> :main
-- 405
-- 61297
main :: IO ()
main = do
    input <- readParsedLines 2023 22 parser

    let cubes = map voxels input
        stack = Map.fromList . concat $ zipWith (\a b -> map (,a) b) [1..] cubes

        after  = fall stack
        bricks = toBricks after

        soleSupport    = map (head . snd) . filter ((== 1) . length . snd) $ support moveDown after
        toDisintegrate = filter (`notElem` soleSupport) $ Map.keys bricks

        forChainReaction = filter (`elem` soleSupport) $ Map.keys bricks

    print $ length toDisintegrate
    print $ chainReactions after forChainReaction
