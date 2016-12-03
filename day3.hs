isTriangle :: [Int] -> Bool
isTriangle [a, b, c] = a + b > c && a + c > b && b + c > a

part1 :: String -> Int
part1 = length . filter (isTriangle . map read . words) . lines
