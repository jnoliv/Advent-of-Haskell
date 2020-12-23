import AdventAPI (readInputDefaults)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq

type Cups = Seq.Seq Int

play :: Int -> Cups -> Cups
play n (cur Seq.:<| c1 Seq.:<| c2 Seq.:<| c3 Seq.:<| cups) =
    (bef Seq.|> c1 Seq.|> c2 Seq.|> c3) Seq.>< (aft Seq.|> cur)
    where next x     = if x > 1 then pred x else n
          dest       = until (`notElem` [c1,c2,c3]) next (next cur)
          destIdx    = succ . fromJust $ Seq.elemIndexL dest cups
          (bef, aft) = Seq.splitAt destIdx cups

order :: Cups -> Cups
order cups = aft Seq.>< bef
    where idx = fromJust $ Seq.elemIndexL 1 cups
          (bef, _ Seq.:<| aft) = Seq.splitAt idx cups

main :: IO()
main = do
    cups <- map digitToInt . init <$> readInputDefaults 23

    let n        = maximum cups
        after100 = iterate (play n) (Seq.fromList cups) !! 100

    putStrLn . map intToDigit . toList . order $ after100
