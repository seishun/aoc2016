import Prelude hiding (lookup, filter)
import Data.Map hiding (foldl, map)

type State = (Map Int (Int, Int), Map Int (Int, Int))

give :: Int -> Int -> State -> State
give bot value (bots, tos) =
  case lookup bot bots of
    Just (value', -1) ->
      let state' = (insert bot (value', value) bots, tos)
      in case lookup bot tos of
        Just (toLower, toHigher) -> overflow value value' toLower toHigher state'
        Nothing -> state'
    Nothing -> (insert bot (value, -1) bots, tos)

overflow :: Int -> Int -> Int -> Int -> State -> State
overflow value1 value2 toLower toHigher =
  let higher = max value1 value2
      lower = min value1 value2
  in give toHigher higher . give toLower lower

parse :: [String] -> State -> State
parse ["value", value, "goes", "to", "bot", bot] state =
  let value' = read value
      bot' = read bot
  in give bot' value' state
parse ["bot", bot, "gives", "low", "to", "bot", toLower, "and", "high", "to", "bot", toHigher] (bots, tos) =
  let bot' = read bot
      toLower' = read toLower
      toHigher' = read toHigher
      state' = (bots, insert bot' (toLower', toHigher') tos)
  in case lookup bot' bots of
    Just (_, -1) -> state'
    Just (value, value') -> overflow value value' toLower' toHigher' state'
    Nothing -> state'
parse ["bot", bot, "gives", "low", "to", "output", toLower, "and", "high", "to", "bot", toHigher] (bots, tos) =
  let bot' = read bot
      toLower' = -read toLower - 1 -- ugly hack, use negated index minus one to denote outputs
      toHigher' = read toHigher
      state' = (bots, insert bot' (toLower', toHigher') tos) 
  in case lookup bot' bots of
    Just (_, -1) -> state'
    Just (value, value') -> overflow value value' toLower' toHigher' state'
    Nothing -> state'
parse ["bot", bot, "gives", "low", "to", "output", toLower, "and", "high", "to", "output", toHigher] (bots, tos) =
  let bot' = read bot
      toLower' = -read toLower - 1
      toHigher' = -read toHigher - 1
      state' = (bots, insert bot' (toLower', toHigher') tos)
  in case lookup bot' bots of
    Just (_, -1) -> state'
    Just (value, value') -> overflow value value' toLower' toHigher' state'
    Nothing -> state'

part1 :: String -> Int
part1 = head . keys . filter (`elem` [(61, 17), (17, 61)]) . fst . foldl (flip parse) (empty, empty) . map words . lines

part2 :: String -> Int
part2 input =
  let (outputs, _) = foldl (flip parse) (empty, empty) . map words . lines $ input
      (output0, _) = outputs ! (-1)
      (output1, _) = outputs ! (-2)
      (output2, _) = outputs ! (-3)
  in output0 * output1 * output2
