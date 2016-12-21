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

basedRotate' :: Char -> Seq Char -> Seq Char
basedRotate' x string =
  let (Just index) = elemIndexL x string
      times = [7,7,2,6,1,5,0,4] !! index
  in rotateRight times string

reversePos :: Int -> Int -> Seq Char -> Seq Char
reversePos x y string =
  let slice = reverse $ drop x $ take (y + 1) string
  in take x string >< slice >< drop (y + 1) string

move :: Int -> Int -> Seq Char -> Seq Char
move x y string =
  let x' = string `index` x
  in insertAt y x' $ deleteAt x string

parse :: Bool -> [String] -> Seq Char -> Seq Char
parse _ ["swap", "position", x, "with", "position", y] = swapPos (read x) (read y)
parse _ ["swap", "letter", [x], "with", "letter", [y]] = swapLetter x y
parse u ["rotate", "left", x, _] = (if u then rotateRight else rotateLeft) (read x)
parse u ["rotate", "right", x, _] = (if u then rotateLeft else rotateRight) (read x)
parse u ["rotate", "based", "on", "position", "of", "letter", [x]] = (if u then basedRotate' else basedRotate) x
parse _ ["reverse", "positions", x, "through", y] = reversePos (read x) (read y)
parse u ["move", "position", x, "to", "position", y] = (if u then flip move else move) (read x) (read y)

part1 :: String -> String
part1 = toList . foldl (flip ($)) (fromList "abcdefgh") . map (parse False . words) . lines

part2 :: String -> String
part2 = toList . foldr ($) (fromList "fbgdceah") . map (parse True . words) . lines
