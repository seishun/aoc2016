import Prelude hiding (drop, length, reverse, take)
import Data.Foldable (toList)
import Data.Sequence

swapPos :: Int -> Int -> Seq Char -> Seq Char
swapPos x y string =
  let x' = string `index` x
      y' = string `index` y
  in update x y' $ update y x' string

swapLetter :: Char -> Char -> Seq Char -> Seq Char
swapLetter x y string = do
  letter <- string
  if letter == x then return y
  else if letter == y then return x
  else return letter

rotateLeft :: Int -> Seq Char -> Seq Char
rotateLeft x string =
  let x' = x `mod` length string
  in drop x' string >< take x' string

rotateRight :: Int -> Seq Char -> Seq Char
rotateRight x string =
  let x' = length string - x
  in rotateLeft x' string

basedRotate :: Char -> Seq Char -> Seq Char
basedRotate x string =
  let (Just index) = elemIndexL x string
      times = 1 + index + fromEnum (index >= 4)
  in rotateRight times string

reversePos :: Int -> Int -> Seq Char -> Seq Char
reversePos x y string =
  let slice = reverse $ drop x $ take (y + 1) string
  in take x string >< slice >< drop (y + 1) string

move :: Int -> Int -> Seq Char -> Seq Char
move x y string =
  let x' = string `index` x
  in insertAt y x' $ deleteAt x string

parse :: [String] -> Seq Char -> Seq Char
parse ["swap", "position", x, "with", "position", y] = swapPos (read x) (read y)
parse ["swap", "letter", [x], "with", "letter", [y]] = swapLetter x y
parse ["rotate", "left", x, _] = rotateLeft (read x)
parse ["rotate", "right", x, _] = rotateRight (read x)
parse ["rotate", "based", "on", "position", "of", "letter", [x]] = basedRotate x
parse ["reverse", "positions", x, "through", y] = reversePos (read x) (read y)
parse ["move", "position", x, "to", "position", y] = move (read x) (read y)

part1 :: String -> String
part1 = toList . foldl (flip ($)) (fromList "abcdefgh") . map (parse . words) . lines
