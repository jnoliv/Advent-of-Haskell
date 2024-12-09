import Advent.API (readInputDefaults)
import Data.Char (digitToInt)
import Data.List (elemIndex, group)
import Data.Maybe (fromMaybe)

data File = Empty Int    -- length
          | File Int Int -- id, length
    deriving (Eq, Show)

unroll :: [Int] -> [Maybe Int]
unroll = file 0
    where
        file _     [] = []
        file n (f:fs) = replicate f (Just n) ++ free (n+1) fs
        free _     [] = []
        free n (f:fs) = replicate f  Nothing ++ file  n    fs

defrag :: [Maybe Int] -> [Int]
defrag []             = []
defrag [Nothing]      = []
defrag (Just n  : fs) = n : defrag fs
defrag (Nothing : fs) = case last fs of
    Just n  -> n : defrag           (init fs)
    Nothing ->     defrag (Nothing : init fs)

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

chunk :: [Maybe Int] -> [File]
chunk = setId 0 . map toFile . group
    where
        toFile fs@(Nothing : _) = Empty  (length fs)
        toFile fs@(Just n  : _) = File 0 (length fs)

        setId _ []              = []
        setId n (Empty  s : fs) = Empty  s : setId  n    fs
        setId n (File 0 s : fs) = File n s : setId (n+1) fs

defragChunk :: [File] -> [Int]
defragChunk disk = map (fromMaybe 0) . unroll . defrag disk . reverse . filter isFile $ disk
    where
        defrag :: [File] -> [File] -> [File]
        defrag disk []     = disk
        defrag disk (f:fs) = case findEmpty 0 disk f of
            Just i  -> defrag (moveFile disk f i) fs
            Nothing -> defrag disk                fs

        isFile :: File -> Bool
        isFile (Empty  _) = False
        isFile (File _ _) = True

        findEmpty :: Int -> [File] -> File -> Maybe Int
        findEmpty i []               _            = Nothing
        findEmpty i (File id' _ : fs) f@(File id _)
            | id' == id = Nothing
            | otherwise = findEmpty (i+1) fs f
        findEmpty i (Empty  s'  : fs) f@(File id s)
            | s' < s    = findEmpty (i+1) fs f
            | otherwise = Just i

        moveFile :: [File] -> File -> Int -> [File]
        moveFile disk f@(File _ s) i
            | s == sempty = before ++ [f]                     ++ after'
            | otherwise   = before ++ [f, Empty (sempty - s)] ++ after'
            where
                (before, (Empty sempty) : after) = splitAt i disk

                Just i' = elemIndex f after
                (after1, _ : after2) = splitAt i' after
                after' = after1 ++ [Empty s] ++ after2

        unroll :: [File] -> [Maybe Int]
        unroll []             = []
        unroll (Empty s  : fs) = replicate s Nothing  ++ unroll fs
        unroll (File n s : fs) = replicate s (Just n) ++ unroll fs

showDisk :: [Maybe Int] -> String
showDisk = concatMap (maybe "." show)

-- |
-- >>> :main
-- 6607511583593
-- 6636608781232
main :: IO ()
main = do
    input <- map digitToInt . init <$> readInputDefaults 2024 9

    print . checksum . defrag      .         unroll $ input
    print . checksum . defragChunk . chunk . unroll $ input
