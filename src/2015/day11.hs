{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments #-}

import AdventAPI (readInputDefaults)
import Data.Char (ord, chr)

-- |
-- >>> inc "aa"
-- "ab"
--
-- >>> inc "az"
-- "ba"
--
-- >>> inc "zz"
-- "aa"
inc :: String -> String
inc = reverse . f . reverse
    where
        f []         = []
        f ('z' : cs) = 'a'             : f cs
        f ( c  : cs) = chr (ord c + 1) :   cs


isValid :: String -> Bool
isValid str = hasStraight str && notHasForbidden str && hasTwoPairs str

-- |
-- >>> hasStraight "osdabcjhg"
-- True
--
-- >>> hasStraight "osdasdabc"
-- True
--
-- >>> hasStraight "ovdcaaudh"
-- False
hasStraight :: String -> Bool
hasStraight (a : b : c : tail)
    | (ord a + 2 == ord b + 1) && (ord b + 1 == ord c) = True
    | otherwise                                        = hasStraight (b : c : tail)
hasStraight _ = False

-- |
-- >>> notHasForbidden "adhfjb"
-- True
--
-- >>> notHasForbidden "awei"
-- False
notHasForbidden :: String -> Bool
notHasForbidden str = all (`notElem` str) ['i', 'o', 'l']

-- |
-- >>> hasTwoPairs "aaa"
-- False
--
-- >>> hasTwoPairs "aabcc"
-- True
--
-- >>> hasTwoPairs "sertgvbnjkiuytfdsdf"
-- False
hasTwoPairs :: String -> Bool
hasTwoPairs str = f 0 str
    where
        f 2 _  = True
        f _ [] = False
        f n (a : b : tail)
            | a == b    = f (n + 1)      tail
            | otherwise = f  n      (b : tail)
        f _ _ = False

-- |
-- >>> :main
-- vzbxxyzz
-- vzcaabcc
main :: IO ()
main = do
    pass <- init <$> readInputDefaults 2015 11

    let pass1 = until isValid inc (inc pass)
        pass2 = until isValid inc (inc pass1)

    putStrLn pass1
    putStrLn pass2
