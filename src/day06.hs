import qualified Common.AdventAPI as AdventAPI

import qualified Data.Set as Set
import Data.List.Split (splitOn)

type AnswerGroup = Set.Set Char

parseAnswers :: [String] -> [AnswerGroup]
parseAnswers = map (Set.fromList . filter (/='\n'))

main :: IO()
main = do
    contents <- AdventAPI.readInput 6 "../session-cookie.txt" "../input"

    let groupAnswers = parseAnswers $ splitOn "\n\n" contents
        sumAnswers = sum . map Set.size $ groupAnswers

    print sumAnswers
