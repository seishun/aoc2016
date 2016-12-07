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

supportsSSL :: String -> Bool
supportsSSL ('[':xs) = supportsSSL' xs
supportsSSL (c1:c2:c3:xs)
  | c1 == c3 && c1 /= c2 = hasCorrespondingBab (c1,c2) xs || supportsSSL (c2:c3:xs)
  | otherwise = supportsSSL (c2:c3:xs)
supportsSSL _ = False

supportsSSL' :: String -> Bool
supportsSSL' (']':xs) = supportsSSL xs
supportsSSL' (c1:c2:c3:xs)
  | c1 == c3 && c1 /= c2 = hasCorrespondingAba (c1,c2) xs || supportsSSL' (c2:c3:xs)
  | otherwise = supportsSSL' (c2:c3:xs)
supportsSSL' _ = False

hasCorrespondingBab :: (Char, Char) -> String -> Bool
hasCorrespondingBab ab ('[':xs) = hasCorrespondingBab' ab xs
hasCorrespondingBab ab (x:xs) = hasCorrespondingBab ab xs
hasCorrespondingBab ab _ = False

hasCorrespondingBab' :: (Char, Char) -> String -> Bool
hasCorrespondingBab' ab (']':xs) = hasCorrespondingBab ab xs
hasCorrespondingBab' (a,b) (c1:c2:c3:xs) = [c1,c2,c3] == [b,a,b] || hasCorrespondingBab' (a,b) (c2:c3:xs)
hasCorrespondingBab' _ _ = False

hasCorrespondingAba :: (Char, Char) -> String -> Bool
hasCorrespondingAba ba (']':xs) = hasCorrespondingAba' ba xs
hasCorrespondingAba ba (x:xs) = hasCorrespondingAba ba xs
hasCorrespondingAba ba _ = False

hasCorrespondingAba' :: (Char, Char) -> String -> Bool
hasCorrespondingAba' ba ('[':xs) = hasCorrespondingAba ba xs
hasCorrespondingAba' (b,a) (c1:c2:c3:xs) = [c1,c2,c3] == [a,b,a] || hasCorrespondingAba' (b,a) (c2:c3:xs)
hasCorrespondingAba' _ _ = False

part2 :: String -> Int
part2 = length . filter supportsSSL . lines
