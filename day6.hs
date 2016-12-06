import Data.List

part1 :: String -> String
part1 = map (head . head . reverse . sortOn length . group . sort) . transpose . words

part2 :: String -> String
part2 = map (head . head . sortOn length . group . sort) . transpose . words
