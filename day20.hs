import Data.List

lowest :: Int -> [(Int, Int)] -> Int
lowest candidate [] = candidate
lowest candidate ((lo, hi):xs) =
  if lo > candidate then candidate
  else lowest (max candidate (hi + 1)) xs

parse :: String -> (Int, Int)
parse line =
  let [(lo, '-':hi)] = reads line
  in (lo, read hi)

part1 :: String -> Int
part1 = lowest 0 . sort . map parse . lines
