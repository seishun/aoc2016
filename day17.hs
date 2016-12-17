import Control.Monad
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5
import Data.Maybe

type Path = (String, (Int, Int))

neighbors :: String -> Path -> [Path]
neighbors passcode (path, (x, y)) = do
  let hash = show . md5 . pack $ passcode ++ path
  ((x', y'), door, char) <- zip3 [(x,y-1),(x,y+1),(x-1,y),(x+1,y)] "UDLR" hash
  guard $ char `elem` "bcdef"
  guard $ x' >= 1 && y' >= 1
  guard $ x' <= 4 && y' <= 4
  return (path ++ [door], (x', y'))

bfs :: [Path] -> String -> String
bfs unvisited passcode = case filter ((== (4, 4)) . snd) unvisited of
  [(path, _)] -> path
  [] -> let unvisited' = concatMap (neighbors passcode) unvisited
        in bfs unvisited' passcode

longestPath :: String -> Path -> Maybe Int
longestPath passcode (path, (x, y))
  | (x, y) == (4, 4) = Just $ length path
  | otherwise =
    let paths = mapMaybe (longestPath passcode) $ neighbors passcode (path, (x, y))
    in if null paths then Nothing
    else Just $ maximum paths

part1 :: String -> String
part1 = bfs [("", (1,1))]

part2 :: String -> Int
part2 input = fromJust $ longestPath input ("", (1,1))
