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
