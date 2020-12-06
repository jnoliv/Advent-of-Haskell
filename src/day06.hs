import qualified Common.AdventAPI as AdventAPI

import qualified Data.Set as Set
import Data.List (intersect)
import Data.List.Split (splitOn)

type AnswerGroup = Set.Set Char

parseAnswers1 :: [String] -> [AnswerGroup]
parseAnswers1 = map (Set.fromList . filter (/='\n'))

parseAnswers2 :: [String] -> [String]
parseAnswers2 = map (foldr1 intersect . lines)

main :: IO()
main = do
    contents <- AdventAPI.readInput 6 "../session-cookie.txt" "../input"

    let groups = splitOn "\n\n" contents

    print . sum . map Set.size . parseAnswers1 $ groups
    print . sum . map length . parseAnswers2 $ groups
