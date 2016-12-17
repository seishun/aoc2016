dragon :: String -> String
dragon a =
  let b = map replace $ reverse a
  in a ++ "0" ++ b
  where replace '0' = '1'
        replace '1' = '0'

fill :: Int -> String -> String
fill disk state
  | length state >= disk = take disk state
  | otherwise = fill disk $ dragon state

checksum :: String -> String
checksum string
  | odd $ length string = string
  | otherwise = checksum $ collapse string
    where collapse [] = ""
          collapse (a:b:xs)
            | a == b = '1' : collapse xs
            | a /= b = '0' : collapse xs

part1 :: String -> String
part1 = checksum . fill 272
