import Control.Monad
import Data.List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Map = (String, Int)
type Pos = Int

neighbors :: Map -> Pos -> [Pos]
neighbors (m, width) pos = do
  pos' <- [pos - width, pos - 1, pos + 1, pos + width]
  guard $ pos' >= 0 && pos' < length m
  guard $ m !! pos' /= '#'
  return pos'

bfs :: Map -> Pos -> Set Pos -> Set Pos -> Int
bfs m target visited unvisited =
  if Set.member target unvisited
  then 0
  else let unvisited' = Set.filter (`Set.notMember` visited) $ Set.fromList $ concatMap (neighbors m) unvisited
           visited' = Set.union visited unvisited
  in 1 + bfs m target visited' unvisited'

tsp :: Map -> Int
tsp m = minimum $ map total (permutations "1234567")
  where total = snd . foldl (\(from, dist) to -> (to, dist + distances ! (from, to))) ('0', 0)
        distances = Map.fromList $ do
          from <- "01234567"
          to <- "01234567"
          guard $ from /= to
          if to < from
          then return ((from, to), distances ! (to, from))
          else return ((from, to), distance m from to)

distance :: Map -> Char -> Char -> Int
distance (m, width) from to =
  let posFrom = head $ elemIndices from m
      posTo = head $ elemIndices to m
  in bfs (m, width) posTo Set.empty $ Set.singleton posFrom

part1 :: String -> Int
part1 input =
  let rows = lines input
      width = length $ head rows
  in tsp (concat rows, width)
