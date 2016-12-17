wait :: Int -> [(Int, Int)] -> [(Int, Int)]
wait time discs = do
  (position, positions) <- discs
  let position' = (position + time) `mod` positions
  return (position', positions)

getsCapsule :: [(Int, Int)] -> Bool
getsCapsule [] = True
getsCapsule discs =
  let ((position,_):xs) = wait 1 discs
  in position == 0 && getsCapsule xs

parse :: [String] -> (Int, Int)
parse ["Disc", _, "has", positions, "positions;", "at", "time=0,", "it", "is", "at", "position", position] =
  let positions' = read positions
      [(position', _)] = reads position
  in (position', positions')

part1 :: String -> Int
part1 input =
  let discs = map (parse . words) . lines $ input
  in head [time | time <- [0..], getsCapsule $ wait time discs]

part2 :: String -> Int
part2 input =
  let discs = (map (parse . words) . lines $ input) ++ [(0, 11)]
  in head [time | time <- [0..], getsCapsule $ wait time discs]
