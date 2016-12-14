import Data.ByteString.Lazy.Char8 (pack, unpack, append)
import Data.Digest.Pure.MD5
import Data.List

hashWith :: String -> Int -> String
hashWith salt = show . md5 . append (pack salt) . pack . show

findTriplet :: String -> Maybe Char
findTriplet (c1:c2:c3:xs)
  | c1 == c2 && c2 == c3 = Just c1
  | otherwise = findTriplet (c2:c3:xs)
findTriplet _ = Nothing

hasFiveSequence :: Char -> String -> Bool
hasFiveSequence chr hash = (replicate 5 chr) `isInfixOf` hash

findKeys :: Int -> [String] -> Int
findKeys 0 _ = -1
findKeys count (x:xs) =
  case findTriplet x of
    Nothing -> 1 + findKeys count xs
    Just c -> if any (hasFiveSequence c) $ take 1000 xs
              then 1 + findKeys (count - 1) xs
              else 1 + findKeys count xs

hashes :: String -> [String]
hashes salt = map (hashWith salt) [0..]

part1 :: String -> Int
part1 = findKeys 64 . hashes
