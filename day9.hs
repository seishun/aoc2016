decompress :: String -> String
decompress ('(':xs) =
  let [(chars, 'x':xs')] = reads xs
      [(times, ')':xs'')] = reads xs'
  in concat (replicate times $ take chars xs'') ++ decompress (drop chars xs'')
decompress (x:xs) = [x] ++ decompress xs
decompress [] = []

part1 :: String -> Int
part1 = length . decompress
