{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Advent.Coord.Grid
import Advent.Life
import Advent.Math
import Advent.Utils

import Control.Lens
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Function
import Data.List
import Data.Maybe

import Data.List.Index
import Data.List.Split

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Debug.Trace

parser :: Parser ()
parser = ()

-- |
-- >>> :main
-- 
main :: IO ()
main = do
    input <- readInputDefaults YEAR DAY
    input <- readParsed YEAR DAY parser
    input <- readParsedLines YEAR DAY parser

    print input
