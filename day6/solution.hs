import Data.List (findIndices, nub)

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs
  | length (take n xs) < n = []
  | otherwise = take n xs : slidingWindow n (tail xs)

isMarker :: (Eq a) => Int -> [a] -> Bool
isMarker n = (== n) . length . nub

firstMarker :: (Eq a) => Int -> [a] -> Int
firstMarker n = (+ n) . head . findIndices (isMarker n) . slidingWindow n

main = do
  input <- readFile "input.txt"
  print $ firstMarker 4 input
  print $ firstMarker 14 input
