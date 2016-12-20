import Data.List

lowest :: Int -> [(Int, Int)] -> Int
lowest candidate [] = candidate
lowest candidate ((lo, hi):xs) =
  if lo > candidate then candidate
  else lowest (max candidate (hi + 1)) xs

total :: Int -> [(Int, Int)] -> Int
total candidate [] = 4294967295 - candidate + 1
total candidate ((lo, hi):xs) =
  if lo > candidate then lo - candidate + total (hi + 1) xs
  else total (max candidate (hi + 1)) xs

parse :: String -> (Int, Int)
parse line =
  let [(lo, '-':hi)] = reads line
  in (lo, read hi)

part1 :: String -> Int
part1 = lowest 0 . sort . map parse . lines

part2 :: String -> Int
part2 = total 0 . sort . map parse . lines
