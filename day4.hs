import Data.List

removeDash :: String -> String
removeDash = filter $ \c -> c /= '-'

sectorIdOrZero :: String -> Int
sectorIdOrZero room =
  let (']':c5:c4:c3:c2:c1:'[':d3:d2:d1:xs) = reverse $ removeDash room
      top5 = map head $ take 5 $ reverse $ sortOn length $ group $ reverse $ sort xs
  in if [c1,c2,c3,c4,c5] == top5
  then read [d1,d2,d3]
  else 0

part1 :: String -> Int
part1 = sum . map sectorIdOrZero . lines

