import Data.Complex

direction :: Char -> Complex Float
direction 'U' = 0 :+ 1
direction 'D' = 0 :+ (-1)
direction 'L' = (-1) :+ 0
direction 'R' = 1 :+ 0

clamp :: Float -> Float
clamp = max (-1) . min 1

advance :: Complex Float -> Complex Float -> Complex Float
advance pos dir =
  let (x :+ y) = pos + dir
  in clamp x :+ clamp y

complexToButton :: Complex Float -> Char
complexToButton ((-1) :+   1)  = '1'
complexToButton (  0  :+   1)  = '2'
complexToButton (  1  :+   1)  = '3'
complexToButton ((-1) :+   0)  = '4'
complexToButton (  0  :+   0)  = '5'
complexToButton (  1  :+   0)  = '6'
complexToButton ((-1) :+ (-1)) = '7'
complexToButton (  0  :+ (-1)) = '8'
complexToButton (  1  :+ (-1)) = '9'

part1 :: String -> String
part1 input = do
  line <- lines input
  let button = complexToButton $ foldl advance 0 $ map direction line
  return button

complexToButton2 :: Complex Float -> Char
complexToButton2 (  0  :+   2)  = '1'
complexToButton2 ((-1) :+   1)  = '2'
complexToButton2 (  0  :+   1)  = '3'
complexToButton2 (  1  :+   1)  = '4'
complexToButton2 ((-2) :+   0)  = '5'
complexToButton2 ((-1) :+   0)  = '6'
complexToButton2 (  0  :+   0)  = '7'
complexToButton2 (  1  :+   0)  = '8'
complexToButton2 (  2  :+   0)  = '9'
complexToButton2 ((-1) :+ (-1)) = 'A'
complexToButton2 (  0  :+ (-1)) = 'B'
complexToButton2 (  1  :+ (-1)) = 'C'
complexToButton2 (  0  :+ (-2)) = 'D'
complexToButton2 _              = '0' -- default

advance2 :: Complex Float -> Complex Float -> Complex Float
advance2 pos dir =
  let pos' = pos + dir
  in if complexToButton2 pos' == '0'
  then pos
  else pos'

part2 :: String -> String
part2 input = do
  line <- lines input
  let button = complexToButton2 $ foldl advance2 0 $ map direction line
  return button
