import AdventAPI (readInputDefaults)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (isPrefixOf)
import Numeric (showHex)

pad :: Int -> String -> String
pad l s = replicate (l - length s) '0' ++ s

toHex :: BS.ByteString -> String 
toHex = pad 32 . concat . map (pad 2 . flip showHex "") . BS.unpack

-- |
-- >>> hash "abcdef"  609043
-- "000001dbbfa3a5c83a2d506429c7b00e"
--
-- >>> hash "pqrstuv" 1048970
-- "000006136ef2ff3b291c85725f17325c"
hash :: String -> Int -> String
hash key = toHex . MD5.hash . BSC.pack . (key ++) . show

mine :: Int -> String -> Int
mine lead0 key = until ((prefix `isPrefixOf`) . hash key) succ 0
    where prefix = replicate lead0 '0'

-- |
-- >>> :main
-- 346386
-- 9958218
main :: IO ()
main = do
    key <- init <$> readInputDefaults 2015 4

    print $ mine 5 key
    print $ mine 6 key
