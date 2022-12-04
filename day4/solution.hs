import Data.List

tupleMap :: (String -> a) -> (String, String) -> (a, a)
tupleMap f (x, y) = (f x, f y)

tailOfSnd :: (String, String) -> (String, String)
tailOfSnd (x, y) = (x, tail y)

parseEntry :: String -> ((Int, Int), (Int, Int))
parseEntry s = (parseBounds x, parseBounds y)
  where
    (x, y) = tailOfSnd $ break (== ',') s
    parseBounds :: String -> (Int, Int)
    parseBounds xs = tupleMap (read :: String -> Int) $ tailOfSnd $ break (== '-') xs

fullyContains :: (Int, Int) -> (Int, Int) -> Bool
fullyContains (x1, x2) (y1, y2) = x1 >= y1 && x2 <= y2 || y1 >= x1 && y2 <= x2

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (x1, x2) (y1, y2) = y1 <= x2 && y2 >= x2 || x1 <= y2 && x2 >= y2

-- x1   x2y1

main = do
  input <- readFile "input.txt"
  print $ sum $ map (fromEnum . uncurry fullyContains . parseEntry) $ lines input
  print $ sum $ map (fromEnum . uncurry overlaps . parseEntry) $ lines input
