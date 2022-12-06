import Data.Char (isAlpha)

-- the problem here is parsing the input
-- more specifically, parsing the stacks

-- it's way easier to parse this when we reverse it

type Crate = [Char]

type Move = [Int]

parseMove :: String -> Move
parseMove = map read . words . filter (not . isAlpha)

parseMoves :: [String] -> [Move]
parseMoves = map parseMove

parseCrates :: [String] -> [Crate]
parseCrates s = parseRows crates (replicate n [])
  where
    n = numCrates $ last s
    crates = drop 1 $ reverse s
    parseRows :: [String] -> [Crate] -> [Crate]
    parseRows [] crates = crates
    parseRows (row : rows) crates = parseRows rows $ parseColumn row 0 crates
    parseColumn :: String -> Int -> [Crate] -> [Crate]
    parseColumn ['[', c, ']'] i crates = push1 i crates [c] -- matches the last crate
    parseColumn "   " _ crates = crates -- matches the last empty space
    parseColumn ('[' : c : ']' : ' ' : s) i crates = parseColumn s (i + 1) (push1 i crates [c])
    parseColumn s i crates = parseColumn (drop 4 s) (i + 1) crates

numCrates :: String -> Int
numCrates = read . last . words . filter (not . isAlpha)

-- from the first line, I know how many crates there are

parseInput :: [String] -> ([Crate], [Move])
parseInput input = (parseCrates crates, parseMoves moves)
  where
    (crates, moves) = foldr withAccumulation ([], []) input
    withAccumulation "" (crates, moves) = (crates, moves)
    withAccumulation s@('m' : _) (crates, moves) = (crates, s : moves)
    withAccumulation s (crates, moves) = (s : crates, moves)

popFrom :: Int -> [Crate] -> [Crate]
popFrom s crates =
  take s crates
    ++ [tail (crates !! s)]
    ++ drop (s + 1) crates

type Push = Int -> [Crate] -> Crate -> [Crate]

push1 :: Int -> [Crate] -> Crate -> [Crate]
push1 d crates new =
  take d crates
    ++ [new ++ (crates !! d)]
    ++ drop (d + 1) crates

push2 :: Int -> [Crate] -> Crate -> [Crate]
push2 d crates new =
  take d crates
    ++ [reverse new ++ (crates !! d)]
    ++ drop (d + 1) crates

takeFrom :: Int -> [Crate] -> Char
takeFrom s = head . (!! s)

applyMove :: Push -> [Crate] -> Move -> [Crate]
applyMove push crates [amount, source, dest] = moveFromSource crates amount []
  where
    moveToDest :: [Crate] -> Crate -> [Crate]
    moveToDest = push (dest - 1)
    moveFromSource :: [Crate] -> Int -> Crate -> [Crate]
    moveFromSource crates 0 temp = moveToDest crates temp
    moveFromSource crates n temp = moveFromSource (popFrom (source - 1) crates) (n - 1) (takeFrom (source - 1) crates : temp)

simulate :: Push -> [Crate] -> [Move] -> [Crate]
simulate push = foldl (applyMove push)

main = do
  input <- lines <$> readFile "input.txt"
  print $ map head $ uncurry (simulate push1) $ parseInput input
  print $ map head $ uncurry (simulate push2) $ parseInput input
