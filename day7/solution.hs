import Control.Monad
import Data.List (break, intercalate, sort)
import Data.Maybe
import Data.Set qualified as S

data FS = File Int String | Dir String (Maybe [FS]) deriving (Show)

isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _ = False

type Path = [String]

type State = (Path, FS, Cache) -- FS is always root here

type Cache = S.Set Path

stringToFs :: String -> FS
stringToFs s = case words s of
  ["dir", dirName] -> Dir dirName Nothing
  [fileSize, fileName] -> File (read fileSize) fileName
  _ -> error $ "Cannot convert " ++ s ++ " to FS type."

isCached :: Path -> Cache -> Bool
isCached = S.member

-- mutates state by changin the directory
-- directory change modifies only the current path
changeDirectory :: String -> State -> State
changeDirectory targetDir (path, fs, cache) = (newPath, fs, cache)
  where
    newPath = case targetDir of
      "/" -> ["/"]
      ".." -> let ip = init path in if null ip then ["/"] else ip
      _ -> path ++ [targetDir]

-- adds the files and directories
-- assumes that
addFilesAndDirs :: [String] -> State -> State
addFilesAndDirs lines (path, fs, cache) = (path, newFs, newCache)
  where
    children = map stringToFs lines
    newFs = createChildren path children fs
    newCache = S.insert path cache

-- creates children at a given path.
-- returns the modified filesystem.
-- only creates children if:
--  given path is a directory
--  directory has no children from before
createChildren :: Path -> [FS] -> FS -> FS
createChildren [t] tc dir@(Dir name Nothing) = if t == name then Dir name $ Just tc else dir
createChildren (s : sx) tc dir@(Dir n (Just c))
  | n == s = Dir n (Just $ map (createChildren sx tc) c)
  | otherwise = dir
createChildren _ _ x = x

parseCommands :: [String] -> State -> State
parseCommands [] state = state
parseCommands all@(line : lines) state@(path, _, cache) = case words line of
  ["$", "cd", targetDir] -> parseCommands lines $ changeDirectory targetDir state
  ["$", "ls"] ->
    let (ls, rest) = break ((== '$') . head) lines
        newState = if isCached path cache then state else addFilesAndDirs ls state
     in parseCommands rest newState
  _ -> error $ "unknown command: " ++ line

getSize :: FS -> Int
getSize (File s _) = s
getSize (Dir _ mc) = maybe 0 (sum . map getSize) mc

-- retrieves the filesystem rooted at the given path
matchPath :: Path -> FS -> Maybe FS
matchPath [tn] x =
  let n = case x of
        File _ n -> n
        Dir n _ -> n
   in if tn == n then Just x else Nothing
matchPath (s : sx) (File _ _) = Nothing
matchPath (s : sx) (Dir n (Just c)) =
  if n == s
    then listToMaybe (mapMaybe (matchPath sx) c)
    else Nothing
matchPath _ _ = Nothing

main = do
  input <- lines <$> readFile "input.txt"
  let (_, root, cache) = parseCommands input (["/"], stringToFs "dir /", S.empty)
  let sizes = map getSize $ filter isDir $ mapMaybe (`matchPath` root) (S.elems cache)
  print $ sum $ filter (<= 100000) sizes
  let minSpace = getSize root - 40000000
  print $ head $ dropWhile (< minSpace) $ sort sizes
