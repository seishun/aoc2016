import Control.Monad
import Data.Bits
import Data.Set (Set)
import qualified Data.Set as Set

type Coordinate = (Int, Int)

isOpenSpace :: Int -> Coordinate -> Bool
isOpenSpace favorite (x, y) =
  let sum = x*x + 3*x + 2*x*y + y + y*y + favorite
      bits = popCount sum
  in bits `mod` 2 == 0

neighbors :: Int -> Coordinate -> [Coordinate]
neighbors favorite (x, y) = do
  (x', y') <- [(x-1,y),(x,y-1),(x,y+1),(x+1,y)]
  guard $ x' >= 0 && y' >= 0
  guard $ isOpenSpace favorite (x', y')
  return (x', y')

dijkstra :: Int -> Coordinate -> Set Coordinate -> Set Coordinate -> Int
dijkstra favorite target visited unvisited =
  if Set.member target unvisited
  then 0
  else let unvisited' = Set.filter (`Set.notMember` visited) $ Set.fromList $ concatMap (neighbors favorite) unvisited
           visited' = Set.union visited unvisited
  in 1 + dijkstra favorite target visited' unvisited'

dijkstra' :: Int -> Int -> Set Coordinate -> Set Coordinate -> Set Coordinate
dijkstra' favorite steps visited unvisited =
  let unvisited' = Set.filter (`Set.notMember` visited) $ Set.fromList $ concatMap (neighbors favorite) unvisited
      visited' = Set.union visited unvisited
  in if steps == 0
  then visited'
  else dijkstra' favorite (steps - 1) visited' unvisited'

part1 :: Int -> Int
part1 input = dijkstra input (31,39) Set.empty (Set.singleton (1,1))

part2 :: Int -> Int
part2 input = Set.size $ dijkstra' input 50 Set.empty (Set.singleton (1,1))
