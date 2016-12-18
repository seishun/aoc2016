import Data.List

nextRow :: String -> String
nextRow previous =
  let isTrap "^^." = '^'
      isTrap ".^^" = '^'
      isTrap "^.." = '^'
      isTrap "..^" = '^'
      isTrap _ = '.'
  in map isTrap $ transpose ["." ++ init previous, previous, tail previous ++ "."]

part1 :: String -> Int
part1 = length . filter (== '.') . concat . take 40 . iterate nextRow
