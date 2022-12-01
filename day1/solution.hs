import Data.List (sortBy)

maxOfSum :: (Num a, Ord a) => [[a]] -> a
maxOfSum = maximum . map sum

sumOfTopThree :: (Num a, Ord a) => [[a]] -> a
sumOfTopThree = sum . take 3 . sortBy (flip compare) . map sum

parseInput :: [String] -> [[Int]]
parseInput = foldr toListOfLists [[]]
  where
    toListOfLists :: String -> [[Int]] -> [[Int]]
    toListOfLists [] xss = [] : xss
    toListOfLists s (xs : xss) = ((read s :: Int) : xs) : xss

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ maxOfSum $ parseInput $ lines input
  print $ sumOfTopThree $ parseInput $ lines input
