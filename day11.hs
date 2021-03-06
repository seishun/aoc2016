import Data.List
import Data.List.Split
import Data.Map hiding (null, foldl, map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

data Item = Elevator | Generator String | Chip String deriving (Eq, Ord)
type State = Map Item Int

isValidFloor :: [Item] -> Bool
isValidFloor floor =
  let generators = [t | Generator t <- floor]
  in if null generators
  then True
  else all (`elem` generators) [t | Chip t <- floor]

neighbors :: State -> [State]
neighbors state = do
  let elevatorFloor = state ! Elevator
  let onThisFloor = keys $ Map.filter (== elevatorFloor) state
  x:xs <- tails onThisFloor
  y <- xs
  direction <- [1, -1]
  let newFloor = elevatorFloor + direction
  guard $ newFloor >= 1 && newFloor <= 4
  let newState = mapWithKey (\i f -> if i `elem` [x, y, Elevator] then newFloor else f) state
  guard $ all (isValidFloor . keys . flip Map.filter newState) [(== elevatorFloor), (== newFloor)]
  return newState

dijkstra :: State -> Set State -> Set State -> Int
dijkstra target visited unvisited =
  if Set.member target unvisited
  then 0
  else let unvisited' = Set.filter (`Set.notMember` visited) $ Set.fromList $ concatMap neighbors unvisited
           visited' = Set.union visited unvisited
  in 1 + dijkstra target visited' unvisited'

parseItem :: [String] -> [Item]
parseItem ("a":t:"generator,":xs) = Generator t : parseItem xs
parseItem ("a":tc:"microchip,":xs) = Chip (head $ splitOn "-" tc) : parseItem xs
parseItem ("a":tc:"microchip":xs) = Chip (head $ splitOn "-" tc) : parseItem xs
parseItem ("and":xs) = parseItem xs
parseItem ["a",t,"generator."] = [Generator t]
parseItem ["a",tc,"microchip."] = [Chip $ head $ splitOn "-" tc]
parseItem ["nothing","relevant."] = []

parse :: State -> [String] -> State
parse state ("The":"first":"floor":"contains":xs) = Map.union state $ fromList [(k, 1) | k <- parseItem xs]
parse state ("The":"second":"floor":"contains":xs) = Map.union state $ fromList [(k, 2) | k <- parseItem xs]
parse state ("The":"third":"floor":"contains":xs) = Map.union state $ fromList [(k, 3) | k <- parseItem xs]
parse state ("The":"fourth":"floor":"contains":xs) = Map.union state $ fromList [(k, 4) | k <- parseItem xs]

part1 :: String -> Int
part1 input =
  let initial = foldl parse (Map.fromList [(Elevator, 1)]) $ map words $ lines input
      target = Map.map (const 4) initial
  in dijkstra target Set.empty (Set.singleton initial)

part2 :: String -> Int
part2 input =
  let firstFloor = [Elevator, Generator "elerium", Chip "elerium", Generator "dilithium", Chip "dilithium"]
      initial = foldl parse (Map.fromList [(k, 1) | k <- firstFloor]) $ map words $ lines input
      target = Map.map (const 4) initial
  in dijkstra target Set.empty (Set.singleton initial)
