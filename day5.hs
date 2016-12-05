import Data.ByteString.Lazy.Char8 (pack, unpack, append)
import Data.Digest.Pure.MD5

hashWith :: String -> Int -> String
hashWith id = show . md5 . append (pack id) . pack . show 

part1 :: String -> String
part1 id = map (!! 5) $ take 8 $ filter (\s -> take 5 s == "00000") $ map (hashWith id) [0..]

password :: String -> [String] -> String
password ['_',c1,c2,c3,c4,c5,c6,c7] (('0':c:_):xs) = password [c,c1,c2,c3,c4,c5,c6,c7] xs
password [c0,'_',c2,c3,c4,c5,c6,c7] (('1':c:_):xs) = password [c0,c,c2,c3,c4,c5,c6,c7] xs
password [c0,c1,'_',c3,c4,c5,c6,c7] (('2':c:_):xs) = password [c0,c1,c,c3,c4,c5,c6,c7] xs
password [c0,c1,c2,'_',c4,c5,c6,c7] (('3':c:_):xs) = password [c0,c1,c2,c,c4,c5,c6,c7] xs
password [c0,c1,c2,c3,'_',c5,c6,c7] (('4':c:_):xs) = password [c0,c1,c2,c3,c,c5,c6,c7] xs
password [c0,c1,c2,c3,c4,'_',c6,c7] (('5':c:_):xs) = password [c0,c1,c2,c3,c4,c,c6,c7] xs
password [c0,c1,c2,c3,c4,c5,'_',c7] (('6':c:_):xs) = password [c0,c1,c2,c3,c4,c5,c,c7] xs
password [c0,c1,c2,c3,c4,c5,c6,'_'] (('7':c:_):xs) = password [c0,c1,c2,c3,c4,c5,c6,c] xs
password s (x:xs)
  | '_' `elem` s = password s xs
  | otherwise = s

part2 :: String -> String
part2 id = password "________" $ map (drop 5) $ filter (\s -> take 5 s == "00000") $ map (hashWith id) [0..]
