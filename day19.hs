import Prelude hiding (drop, null)
import Data.Sequence

lastElf :: Seq Int -> Int
lastElf elves =
  let (elf :< xs) = viewl elves
  in if null xs
  then elf
  else lastElf $ drop 1 xs |> elf

part1 :: Int -> Int
part1 number = lastElf $ fromList [1..number]
