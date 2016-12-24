import Data.Foldable (toList)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Sequence as Seq

data Arg = Register Char | Literal Int
data Instruction = Cpy Arg Arg | Inc Char | Dec Char | Jnz Arg Arg | Tgl Char

execute :: Int -> Map Char Int -> Seq Instruction -> Int
execute ip registers instructions = case instructions !? ip of
  Nothing -> registers ! 'a'
  Just instruction -> case instruction of
    Cpy x (Register y) -> case toList $ Seq.take 6 $ Seq.drop ip instructions of
      [Cpy b1 (Register c1), Inc a1, Dec c2, Jnz (Register c3) (Literal (-2)), Dec d1, Jnz (Register d2) (Literal (-5))] ->
        if c1 == c2 && c2 == c3 && d1 == d2
        then execute (ip + 6) (Map.insert d1 0 $ Map.insert c1 0 $ Map.adjust (+ value b1 * registers ! d1) a1 registers) instructions
        else execute (ip + 1) (Map.insert y (value x) registers) instructions
      _ -> execute (ip + 1) (Map.insert y (value x) registers) instructions
    Cpy _ (Literal _) -> execute (ip + 1) registers instructions
    Inc x -> execute (ip + 1) (Map.adjust succ x registers) instructions
    Dec x -> execute (ip + 1) (Map.adjust pred x registers) instructions
    Jnz x y -> case value x of
      0 -> execute (ip + 1) registers instructions
      _ -> execute (ip + value y) registers instructions
    Tgl x -> execute (ip + 1) registers $ adjust' toggle (ip + registers ! x) instructions
    where toggle (Inc x) = Dec x
          toggle (Dec x) = Inc x
          toggle (Tgl x) = Inc x
          toggle (Jnz x y) = Cpy x y
          toggle (Cpy x y) = Jnz x y
    where value (Literal x) = x
          value (Register x) = registers ! x

parseArg :: String -> Arg
parseArg "a" = Register 'a'
parseArg "b" = Register 'b'
parseArg "c" = Register 'c'
parseArg "d" = Register 'd'
parseArg x = Literal $ read x

parse :: [String] -> Instruction
parse ["cpy", x, [y]] = Cpy (parseArg x) (Register y)
parse ["inc", [x]] = Inc x
parse ["dec", [x]] = Dec x
parse ["jnz", x, y] = Jnz (parseArg x) (parseArg y)
parse ["tgl", [x]] = Tgl x

part1 :: String -> Int
part1 = execute 0 (Map.fromList (('a', 7) : [(c, 0) | c <- "bcd"])) . Seq.fromList . map (parse . words) . lines

part2 :: String -> Int
part2 = execute 0 (Map.fromList (('a', 12) : [(c, 0) | c <- "bcd"])) . Seq.fromList . map (parse . words) . lines
