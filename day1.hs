import Data.Complex
import Data.List
import Data.Maybe

direction :: Char -> Complex Float
direction 'L' = 0 :+ 1
direction 'R' = 0 :+ (-1)

blocks :: String -> Float
blocks str =
  let [(count, _)] = reads str
  in count

parse :: String -> [(Complex Float, Float)]
parse = map (\(x:xs) -> (direction x, blocks xs)) . words

advance :: (Complex Float, Complex Float) -> (Complex Float, Float) -> (Complex Float, Complex Float)
advance (dir, pos) (turn, steps) =
  let dir' = dir * turn
      pos' = pos + dir' * (steps :+ 0)
  in (dir', pos')

part1 :: String -> Int
part1 input =
  let parts = parse input
      (_, (x :+ y)) = foldl advance (0 :+ 1, 0 :+ 0) parts
  in grid x + grid y
  where grid = round . abs

firstTwice :: (Complex Float, Complex Float) -> [(Complex Float, Float)] -> [Complex Float] -> Complex Float
firstTwice (dir, pos) (x:xs) prev =
  let (turn, steps) = x
      (dir':_, poss@(pos':_)) = unzip $ map (advance (dir, pos)) $ map (\s -> (turn, s)) $ reverse [1..steps]
  in fromMaybe (firstTwice (dir', pos') xs (poss ++ prev)) (listToMaybe $ intersect poss prev)

part2 :: String -> Int
part2 input =
  let parts = parse input
      (x :+ y) = firstTwice (0 :+ 1, 0 :+ 0) parts []
  in grid x + grid y
  where grid = round . abs
