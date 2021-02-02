{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Data.List (isPrefixOf, nub, sortOn)
import Data.Maybe (fromJust, mapMaybe, listToMaybe)
import Data.HashSet qualified as Set

format :: Parser ([(String, String)], String)
format = (,) <$> (replacement `endBy` "\n") <* "\n"
             <*> chemical
    where
        chemical    = some letterChar
        replacement = (,) <$> chemical <* " => "
                          <*> chemical

-- | The list of all strings resulting from replacing once each
-- occurence of 'pat' with 'rep' in 'src'
--
-- >>> replacements "aa" "bbb" "cdefaaghikalaaa"
-- ["cdefaaghikalabbb","cdefaaghikalbbba","cdefbbbghikalaaa"]
--
-- >>> nub $ concatMap (replacements "bbb" "aa") ["cdefaaghikalabbb","cdefaaghikalbbba","cdefbbbghikalaaa"]
-- ["cdefaaghikalaaa"]
replacements :: String -> String -> String -> [String]
replacements pat rep src = f [] src []
    where
        n = length pat

        f _   []   acc = acc
        f pre rest acc = f (pre ++ [head rest]) (tail rest)
            (if pat `isPrefixOf` rest then (pre ++ rep ++ (drop n rest)) : acc else acc)

calibrate :: [(String, String)] -> String -> Int
calibrate repls src = Set.size . Set.fromList $ concat [replacements pat rep src | (pat, rep) <- repls]

-- | Calculate the number of replacements that need to be applied to go
-- from 'start' to 'end', using the replacements in 'repls'. This is a
-- greedy algorithm, longer replacements are tried first and there's no
-- guarantee that the result will be the minimum number of steps.
--
-- >>> fabricationSteps [("e","ee")] "e" "eeeeeeeeeee"
-- 10
fabricationSteps :: [(String, String)] -> String -> String -> Int
fabricationSteps repls start end = fromJust $ f 0 end
    where
        nub'   = Set.toList . Set.fromList
        replsO = reverse $ sortOn (length . snd) repls

        f steps cur
            | cur == start    = Just steps
            | otherwise       = listToMaybe $ mapMaybe (f (steps + 1)) reductions
            where
                reductions = nub' $ concat [replacements pat rep cur | (rep, pat) <- replsO]

-- |
-- >>> :main
-- 576
-- 207
main :: IO ()
main = do
    (repls, medicine) <- readParsed 2015 19 format

    print $ calibrate repls medicine
    print $ fabricationSteps repls "e" medicine
