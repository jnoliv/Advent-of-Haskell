{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Data.List (intercalate)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

data Cmd  = CD String | LS [File]
data File = Dir String | File Integer String

data DirTree = DirTree Integer [String]

type State = ([String], Map String DirTree)

format :: Parser [Cmd]
format = some cmd
    where
        cmd = (CD <$> ("$ cd " *> untilLF) <* "\n")
          <|> (LS <$> ("$ ls\n"  *> file `endBy1` "\n"))
        
        file = (Dir  <$> ("dir " *> some letterChar))
           <|> (File <$> decimal <* " " <*> untilLF)

        untilLF = manyTill asciiChar (lookAhead "\n")

makeDirectories :: State -> Cmd -> State
makeDirectories (dirStack, m) (CD "..")  = (tail  dirStack, m)
makeDirectories (dirStack, m) (CD dir )  = (dir : dirStack, m)
makeDirectories (dirStack, m) (LS files) = (      dirStack, Map.insert wd tree m)
    where
        fileName ["/"] = "/"
        fileName s     = intercalate "/" (reverse s)

        wd   = fileName dirStack
        tree = foldl makeDirectory (DirTree 0 []) files

        makeDirectory (DirTree n l) (Dir d)    = DirTree n       ((wd ++ "/" ++ d) : l)
        makeDirectory (DirTree n l) (File s _) = DirTree (n + s) l

countSizes :: Map String DirTree -> Map String Integer
countSizes m = Map.mapWithKey (\k _ -> size m k) m

size :: Map String DirTree -> String -> Integer
size m dir = foldl (\c k -> c + size m k) n dirs
    where
        (DirTree n dirs) = fromJust $ Map.lookup dir m

-- |
-- >>> :main
-- 1427048
-- 2940614
main :: IO ()
main = do
    input <- readParsed 2022 7 format

    let directories = snd $ foldl makeDirectories ([], Map.empty) input
        sizes       = countSizes directories

        atMost100000 = Map.filter (<= 100000) sizes

    let toFree     = (fromJust $ Map.lookup "/" sizes) - 40000000
        candidates = Map.filter (>= toFree) sizes
    
    print $ Map.foldl (+) 0 atMost100000
    print $ Map.foldl min 7000000 candidates
