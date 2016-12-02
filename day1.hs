import Data.Complex

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

day1 :: String -> Int
day1 input =
  let parts = parse input
      (_, (x :+ y)) = foldl advance (0 :+ 1, 0 :+ 0) parts
  in grid x + grid y
  where grid = round . abs

