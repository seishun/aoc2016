import Prelude hiding (drop, length, null)
import Data.Sequence

lastElf :: Seq Int -> Int
lastElf elves =
  let (elf :< xs) = viewl elves
  in if null xs
  then elf
  else lastElf $ drop 1 xs |> elf

lastElf' :: Seq Int -> Int
lastElf' elves =
  let (elf :< xs) = viewl elves
  in if null xs
  then elf
  else lastElf' $ deleteAt (length elves `div` 2 - 1) xs |> elf

part1 :: Int -> Int
part1 number = lastElf $ fromList [1..number]

part2 :: Int -> Int
part2 number = lastElf' $ fromList [1..number]
