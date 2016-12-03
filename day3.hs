import Data.List

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = a + b > c && a + c > b && b + c > a

countTriangles :: [Int] -> Int
countTriangles [] = 0
countTriangles (a:b:c:xs) = countTriangles xs + fromEnum (isTriangle a b c)

part1 :: String -> Int
part1 = countTriangles . map read . words

part2 :: String -> Int
part2 = countTriangles . map read . concat . transpose . map words . lines
