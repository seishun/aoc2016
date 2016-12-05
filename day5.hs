import Data.ByteString.Lazy.Char8 (pack, unpack, append)
import Data.Digest.Pure.MD5

hashWith :: String -> Int -> String
hashWith id = show . md5 . append (pack id) . pack . show 

part1 :: String -> String
part1 id = map (flip (!!) 5) $ take 8 $ filter (\s -> take 5 s == "00000") $ map (hashWith id) [0..]
