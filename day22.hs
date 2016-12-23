import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

data Pos = Pos { x :: Int, y :: Int } deriving (Eq, Ord)
data Node = Node { pos :: Pos
                 , size :: Int
                 , used :: Int
                 } deriving (Eq, Ord)
type State = (Pos, Pos)
data Grid = Grid { wall :: Pos
                 , width :: Int
                 , height :: Int
                 }

avail :: Node -> Int
avail node = size node - used node

viablePairs :: [Node] -> [(Node, Node)]
viablePairs nodes = do
  a <- nodes
  b <- nodes
  guard $ used a > 0
  guard $ pos a /= pos b
  guard $ used a <= avail b
  return (a, b)

neighbors :: Pos -> [Pos]
neighbors (Pos x y) = do
  (x', y') <- [(x-1, y), (x, y-1), (x, y+1), (x+1, y)]
  return $ Pos x' y'

next :: Grid -> State -> [State]
next grid (b, goal) = do
  a <- neighbors b
  guard $ x a >= 0 && x a <= width grid
  guard $ y a >= 0 && y a <= height grid
  guard $ y a /= y (wall grid) || x a < x (wall grid)
  return (a, if goal == a then b else goal)

bfs :: Grid -> Set State -> Set State -> Int
bfs grid visited unvisited =
  if any ((== Pos 0 0) . snd) unvisited
  then 0
  else let unvisited' = Set.filter (`Set.notMember` visited) $ Set.fromList $ concatMap (next grid) unvisited
           visited' = Set.union visited unvisited
  in 1 + bfs grid visited' unvisited'

parse :: [String] -> Node
parse [x, size, used, _, _] =
  let [(x', y)] = reads $ drop (length "/dev/grid/node-x") x
      y' = read $ drop (length "-y") y
      [(size', _)] = reads size
      [(used', _)] = reads used
  in Node (Pos x' y') size' used'

part1 :: String -> Int
part1 = length . viablePairs . map (parse . words) . drop 2 . lines

part2 :: String -> Int
part2 input =
  let nodes = map (parse . words) . drop 2 . lines $ input
      empty = head [pos n | n <- nodes, used n == 0]
      Pos width height = pos $ last nodes
      wall = head [pos n | n <- nodes, size n > 500]
  in bfs (Grid wall width height) Set.empty $ Set.singleton (empty, Pos width 0)
