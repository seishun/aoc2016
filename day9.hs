decompress :: String -> String
decompress ('(':xs) =
  let [(chars, 'x':xs')] = reads xs
      [(times, ')':xs'')] = reads xs'
  in concat (replicate times $ take chars xs'') ++ decompress (drop chars xs'')
decompress (x:xs) = [x] ++ decompress xs
decompress [] = []

decompress_v2 :: String -> Int
decompress_v2 ('(':xs) =
  let [(chars, 'x':xs')] = reads xs
      [(times, ')':xs'')] = reads xs'
  in times * (decompress_v2 $ take chars xs'') + decompress_v2 (drop chars xs'')
decompress_v2 (x:xs) = 1 + decompress_v2 xs
decompress_v2 [] = 0

part1 :: String -> Int
part1 = length . decompress

part2 :: String -> Int
part2 = decompress_v2
