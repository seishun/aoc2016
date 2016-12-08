import Data.List

rect :: Int -> Int -> [String] -> [String]
rect a b screen = map (\row -> replicate a '#' ++ drop a row) (take b screen) ++ drop b screen

shift :: Int -> String -> String
shift a pixels = drop n pixels ++ take n pixels
  where n = length pixels - a

rotateRow :: Int -> Int -> [String] -> [String]
rotateRow a b screen = take a screen ++ [shift b $ screen !! a] ++ tail (drop a screen)

rotateColumn :: Int -> Int -> [String] -> [String]
rotateColumn a b = transpose . rotateRow a b . transpose

parse :: [String] -> [String] -> [String]
parse ["rect", a] =
  let [(a', 'x':b)] = reads a
      b' = read b
  in rect a' b'
parse ["rotate", "row", 'y':'=':a, "by", b] =
  let a' = read a
      b' = read b
  in rotateRow a' b'
parse ["rotate", "column", 'x':'=':a, "by", b] =
  let a' = read a
      b' = read b
  in rotateColumn a' b'

swipe :: String -> [String]
swipe = foldl (flip parse) (replicate 6 $ replicate 50 '.') . map words . lines

part1 :: String -> Int
part1 = length . filter (== '#') . concat . swipe

part2 :: String -> IO ()
part2 = mapM_ putStrLn . swipe
