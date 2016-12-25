import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe

data Arg = Register Char | Literal Int
data Instruction = Cpy Arg Arg | Inc Char | Dec Char | Jnz Arg Arg | Out Arg
type State = (Int, Map Char Int)

execute :: [Instruction] -> State -> Maybe (Maybe Int, State)
execute instructions (ip, registers) =
  if ip >= length instructions
  then Nothing
  else case instructions !! ip of
    Cpy x (Register y) -> case take 6 $ drop ip instructions of
      [Cpy b1 (Register c1), Inc a1, Dec c2, Jnz (Register c3) (Literal (-2)), Dec d1, Jnz (Register d2) (Literal (-5))] ->
        if c1 == c2 && c2 == c3 && d1 == d2
        then Just (Nothing, (ip + 6, Map.insert d1 0 $ Map.insert c1 0 $ Map.adjust (+ value b1 * registers ! d1) a1 registers))
        else Just (Nothing, (ip + 1, Map.insert y (value x) registers))
      _ -> Just (Nothing, (ip + 1, Map.insert y (value x) registers))
    Inc x -> Just (Nothing, (ip + 1, Map.adjust succ x registers))
    Dec x -> Just (Nothing, (ip + 1, Map.adjust pred x registers))
    Jnz x y -> case value x of
      0 -> Just (Nothing, (ip + 1, registers))
      _ -> Just (Nothing, (ip + value y, registers))
    Out x -> Just (Just $ value x, (ip + 1, registers))
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
parse ["out", x] = Out (parseArg x)

isClock :: Int -> [Int] -> Bool
isClock inf signal =
  let clock = cycle [0,1]
  in take inf clock == take inf signal

part1 :: String -> Int
part1 input =
  let instructions = map (parse . words) $ lines input
  in head [i | i <- [1..], isClock 100 $ catMaybes $ unfoldr (execute instructions) (0, Map.fromList $ ('a', i) : [(c, 0) | c <- "bcd"])]
