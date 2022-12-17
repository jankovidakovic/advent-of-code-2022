import Data.List (findIndices, nub, tails)

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs
  | length (take n xs) < n = []
  | otherwise = take n xs : slidingWindow n (tail xs)

betterSlidingWindow :: Int -> [a] -> [[a]]
betterSlidingWindow n = takeWhile ((== n) . length) . map (take n) . tails

isMarker :: (Eq a) => [a] -> Bool
isMarker xs = length (nub xs) == length xs

firstMarker :: (Eq a) => Int -> [a] -> Int
firstMarker n = (+ n) . head . findIndices isMarker . slidingWindow n

main = do
  input <- readFile "input.txt"
  print $ firstMarker 4 input
  print $ firstMarker 14 input
