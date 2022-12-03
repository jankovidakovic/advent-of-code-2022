-- shape : 1 for rock, 2 for paper, 3 for scissors
-- outcome : 0 for loss, 3 for draw, 6 for win
--
numPoints :: String -> Int
numPoints "A X" = 1 + 3
numPoints "A Y" = 2 + 6
numPoints "A Z" = 3 + 0
numPoints "B X" = 1 + 0
numPoints "B Y" = 2 + 3
numPoints "B Z" = 3 + 6
numPoints "C X" = 1 + 6
numPoints "C Y" = 2 + 0
numPoints "C Z" = 3 + 3

-- X = lose, Y = draw, Z = win
-- A = rock, B = paper, C = scissors
numPoints2 :: String -> Int
numPoints2 "A X" = 3 + 0
numPoints2 "A Y" = 1 + 3
numPoints2 "A Z" = 2 + 6
numPoints2 "B X" = 1 + 0
numPoints2 "B Y" = 2 + 3
numPoints2 "B Z" = 3 + 6
numPoints2 "C X" = 2 + 0
numPoints2 "C Y" = 3 + 3
numPoints2 "C Z" = 1 + 6

main = do
  input <- readFile "input.txt"
  print $ sum $ map numPoints $ lines input
  print $ sum $ map numPoints2 $ lines input
