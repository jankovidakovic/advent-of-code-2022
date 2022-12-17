import Data.Char (isAlpha)

type CrateStack = [Char]

type Move = [Int]

parseMove :: String -> Move
parseMove = map read . words . filter (not . isAlpha)

parseMoves :: [String] -> [Move]
parseMoves = map parseMove

-- from the first line, I know how many crates there are
numCrates :: String -> Int
numCrates = read . last . words . filter (not . isAlpha)

parseCrates :: [String] -> [CrateStack]
parseCrates s = parseRows crates (replicate n [])
  where
    n = numCrates $ last s
    crates = drop 1 $ reverse s

    parseRows :: [String] -> [CrateStack] -> [CrateStack]
    parseRows [] crates = crates
    parseRows (row : rows) crates = parseRows rows $ parseColumn row 0 crates

    parseColumn :: String -> Int -> [CrateStack] -> [CrateStack]
    parseColumn ['[', c, ']'] i crates = pushOp1 i crates [c] -- matches the last crate
    parseColumn "   " _ crates = crates -- matches the last empty space
    parseColumn ('[' : c : ']' : ' ' : s) i crates = parseColumn s (i + 1) (pushOp1 i crates [c])
    parseColumn s i crates = parseColumn (drop 4 s) (i + 1) crates

parseInput :: [String] -> ([CrateStack], [Move])
parseInput input = (parseCrates crates, parseMoves moves)
  where
    (crates, moves) = foldr withAccumulation ([], []) input
    withAccumulation "" (crates, moves) = (crates, moves)
    withAccumulation s@('m' : _) (crates, moves) = (crates, s : moves)
    withAccumulation s (crates, moves) = (s : crates, moves)

popFrom :: Int -> [CrateStack] -> [CrateStack]
popFrom s crates =
  take s crates
    ++ [tail (crates !! s)]
    ++ drop (s + 1) crates

type PushOp = Int -> [CrateStack] -> CrateStack -> [CrateStack]

pushOp1 :: Int -> [CrateStack] -> CrateStack -> [CrateStack]
pushOp1 d crates new =
  take d crates
    ++ [new ++ (crates !! d)]
    ++ drop (d + 1) crates

pushOp2 :: Int -> [CrateStack] -> CrateStack -> [CrateStack]
pushOp2 d crates new =
  take d crates
    ++ [reverse new ++ (crates !! d)]
    ++ drop (d + 1) crates

takeFrom :: Int -> [CrateStack] -> Char
takeFrom s = head . (!! s)

applyMove :: PushOp -> [CrateStack] -> Move -> [CrateStack]
applyMove pushTo crates [amount, source, dest] = moveAmount amount crates []
  where
    moveAmount :: Int -> [CrateStack] -> CrateStack -> [CrateStack]
    moveAmount 0 crates temp = pushTo (dest - 1) crates temp
    moveAmount n crates temp = moveAmount (n - 1) (popFrom (source - 1) crates) (takeFrom (source - 1) crates : temp)

simulate :: PushOp -> [CrateStack] -> [Move] -> [CrateStack]
simulate usingPush = foldl (applyMove usingPush)

main = do
  input <- parseInput . lines <$> readFile "input.txt"
  print $ map head $ uncurry (simulate pushOp1) input
  print $ map head $ uncurry (simulate pushOp2) input
