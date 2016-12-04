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

rotate :: Int -> Char -> Char
rotate _ '-' = ' '
rotate id c =
  let i = fromEnum c - fromEnum 'a'
      i' = (i + id) `mod` 26
  in toEnum (i' + fromEnum 'a')

decrypt :: String -> (String, Int)
decrypt room =
  let (']':_:_:_:_:_:'[':d3:d2:d1:xs) = reverse room
      id = read [d1,d2,d3]
  in (map (rotate id) $ reverse xs, id)

part1 :: String -> Int
part1 = sum . map sectorIdOrZero . lines

part2 :: String -> [(String, Int)]
part2 = filter (isInfixOf "north" . fst) . map decrypt . lines
