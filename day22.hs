import Control.Monad

type Node = (String, Int, Int)

viablePairs :: [Node] -> [(Node, Node)]
viablePairs nodes = do
  a@(fsA, usedA, _) <- nodes
  b@(fsB, _, availB) <- nodes
  guard $ usedA > 0
  guard $ fsA /= fsB
  guard $ usedA <= availB
  return (a, b)

parse :: [String] -> Node
parse [fs, _, used, avail, _] =
  let [(used', _)] = reads used
      [(avail', _)] = reads avail
  in (fs, used', avail')

part1 :: String -> Int
part1 = length . viablePairs . map (parse . words) . drop 2 . lines
