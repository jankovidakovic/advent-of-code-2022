import Data.Char
import Data.List

duplicateLetter :: String -> Char
duplicateLetter xs = head [x | x <- xs, x `elem` ys, x `elem` zs]
  where
    (ys, zs) = splitAt (length xs `div` 2) xs

priority :: Char -> Int
priority x
  | x `elem` ['A' .. 'Z'] = y - 38
  | otherwise = y - 96
  where
    y = ord x

commonChar :: (String, String, String) -> Char
commonChar (xs, ys, zs) = head [x | x <- xs, x `elem` xs, x `elem` ys, x `elem` zs]

takeEachThird :: [a] -> [a]
takeEachThird xs = [x | (x, i) <- zip xs [0 ..], i `mod` 3 == 0]

makeTriplets :: [String] -> [(String, String, String)]
makeTriplets xs = zip3 xs (tail xs) (tail $ tail xs)

main = do
  input <- readFile "input.txt"
  print $ sum $ map (priority . duplicateLetter) $ lines input
  print $ sum $ map (priority . commonChar) $ takeEachThird $ makeTriplets $ lines input
