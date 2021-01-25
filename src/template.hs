{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, TupleSections #-}

import AdventAPI (readInputDefaults)
import Advent.Megaparsec
import Advent.Coord.Grid
import Advent.Life
import Advent.Math
import Advent.Utils

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- |
-- >>> :main
-- 
main :: IO ()
main = do
    -- input <- readInputDefaults YEAR DAY
    -- input <- readParsed YEAR DAY format
    -- input <- readParsedLines YEAR DAY format

    print "TODO"
