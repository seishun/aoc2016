import Control.Applicative
import Data.Maybe

hasAbba :: String -> Maybe Bool
hasAbba ('[':xs) = hasAbba' xs
hasAbba (c1:c2:c3:c4:xs)
  | c1 == c4 && c2 == c3 && c1 /= c2 = hasAbba (c2:c3:c4:xs) <|> Just True
  | otherwise = hasAbba (c2:c3:c4:xs)
hasAbba _ = Nothing

hasAbba' :: String -> Maybe Bool
hasAbba' (']':xs) = hasAbba xs
hasAbba' (c1:c2:c3:c4:xs)
  | c1 == c4 && c2 == c3 && c1 /= c2 = Just False
  | otherwise = hasAbba' (c2:c3:c4:xs)
hasAbba' _ = Nothing

part1 :: String -> Int
part1 = length . filter (fromMaybe False . hasAbba) . lines
