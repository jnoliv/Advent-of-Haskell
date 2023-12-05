{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Advent.Coord.Grid
import Advent.Life
import Advent.Math
import Advent.Utils

import Control.Lens
import Data.Bifunctor
import Data.Char
import Data.Function
import Data.List
import Data.Maybe

--import Data.List.Index
--import Data.List.Split

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Debug.Trace

-- |
-- >>> :main
-- 
main :: IO ()
main = do
    input <- readInputDefaults YEAR DAY
    input <- readParsed YEAR DAY format
    input <- readParsedLines YEAR DAY format

    print input
