{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec

import Data.List
import Data.Maybe

import Data.Map (Map)
import Data.Map qualified as Map

import Debug.Trace

data Cmd  = CD String | LS [File]
    deriving Show
data File = Dir String | File Integer String
    deriving Show

data DirTree = DirTree Integer [String]
    deriving Show

format :: Parser [Cmd]
format = cmd `endBy1` "\n"
    where
        cmd :: Parser Cmd
        cmd  = (CD <$> ("$ cd " *> stringTillEOL))
           <|> (LS <$> ("$ ls" *> manyTill ("\n" *> file) (lookAhead newCmdOrEOF)))

        file :: Parser File
        file = (Dir  <$> ("dir " *> some letterChar))
           <|> (File <$> decimal <* " " <*> stringTillEOL)
        
        stringTillEOL = manyTill asciiChar (lookAhead "\n")
        newCmdOrEOF   = try ("\n$") <|> try ("\n" <* eof)

makeTree :: [Cmd] -> Map String DirTree
makeTree cmds = makeTree' cmds [] Map.empty

makeTree' :: [Cmd] -> [String] -> Map String DirTree -> Map String DirTree
makeTree' [] _ m = m

makeTree' ((CD "..") : cmds) dirStack m = makeTree' cmds (tail dirStack)  m
makeTree' ((CD dir ) : cmds) dirStack m = makeTree' cmds (dir : dirStack) m

makeTree' ((LS files) : cmds) s@(dir : dirStack) m = makeTree' cmds s (Map.insert name tree m)
    where
        name = if dirStack == [] then "/" else intercalate "/" . reverse $ s
        tree = foldl f (DirTree 0 []) files

        f (DirTree n l) (Dir d)    = DirTree n ((name ++ "/" ++ d) : l)
        f (DirTree n l) (File s _) = DirTree (n + s) l

size :: Map String DirTree -> String -> Integer
size m dir = foldl (\c k -> c + size m k) n dirs
    where
        (DirTree n dirs) = fromJust $ Map.lookup dir m

countSizes :: Map String DirTree -> Map String Integer
countSizes m = Map.mapWithKey (\k _ -> size m k) m

-- |
-- >>> :main
-- 1427048
-- 
main :: IO ()
main = do
    input <- readParsed 2022 7 format

    let fakeTree = makeTree input
        sizes    = countSizes fakeTree

        atMost100000 = Map.filter (\n -> n <= 100000) sizes

    print $ Map.foldl (+) 0 atMost100000

    let toFree = (fromJust $ Map.lookup "/" sizes) - 40000000
        candidates = Map.filter (\n -> n >= toFree) sizes
    
    print . snd $ Map.foldrWithKey (\k n (mk, mn) -> if n < mn then (k, n) else (mk, mn)) ("", 70000000) candidates
