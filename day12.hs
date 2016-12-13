import Data.Map hiding (map)

data From = Register Char | Literal Int
data Instruction = Cpy From Char | Inc Char | Dec Char | Jnz From Int

execute :: Int -> Map Char Int -> [Instruction] -> Int
execute ip registers instructions
  | ip >= length instructions = registers ! 'a'
  | otherwise = case instructions !! ip of
    Cpy (Register x) y -> execute (ip + 1) (insert y (registers ! x) registers) instructions
    Cpy (Literal x) y -> execute (ip + 1) (insert y x registers) instructions
    Inc x -> execute (ip + 1) (adjust succ x registers) instructions
    Dec x -> execute (ip + 1) (adjust pred x registers) instructions
    Jnz (Register x) y -> case registers ! x of
      0 -> execute (ip + 1) registers instructions
      _ -> execute (ip + y) registers instructions
    Jnz (Literal x) y -> case x of
      0 -> execute (ip + 1) registers instructions
      _ -> execute (ip + y) registers instructions

parse :: [String] -> Instruction
parse ["cpy", x, [y]] = case x of
  "a" -> Cpy (Register 'a') y
  "b" -> Cpy (Register 'b') y
  "c" -> Cpy (Register 'c') y
  "d" -> Cpy (Register 'd') y
  _ -> Cpy (Literal $ read x) y
parse ["inc", [x]] = Inc x
parse ["dec", [x]] = Dec x
parse ["jnz", x, y] = let y' = read y in case x of
  "a" -> Jnz (Register 'a') y'
  "b" -> Jnz (Register 'b') y'
  "c" -> Jnz (Register 'c') y'
  "d" -> Jnz (Register 'd') y'
  _ -> Jnz (Literal $ read x) y'

part1 :: String -> Int
part1 = execute 0 (fromList [(c, 0) | c <- "abcd"]) . map (parse . words) . lines
