import qualified Data.Map as Map

type Update = [Int]
type Rule = (Int,Int)

main = do
  contents <- readFile "input.txt"
  let rules = rulesParser contents
  
  let updates = updateParser $ getUpdates contents
  let updatesRightOrder = map (findRightOrder rules) updates

  let zippedUpdates = zip updates updatesRightOrder

  let rightUpdates = map fst $ filter (uncurry (==)) zippedUpdates
  let faultyUpdatesWhenRight = map snd $ filter (uncurry (/=)) zippedUpdates

  print $ sum $ map getMiddle rightUpdates
  print $ sum $ map getMiddle faultyUpdatesWhenRight

getMiddle :: Update -> Int
getMiddle update = update !! middle
  where middle = length update `div` 2 

findRightOrder ::  [Rule] -> Update -> Update
findRightOrder _ [] = []
findRightOrder rules update = findRightOrder applyingRules newUpdates ++ [last]
  where newUpdates = filter (/= last) update
        last = foldl1 (\num last-> if num `notElem` row1 then num else last) update 
        row1 = map fst applyingRules
        applyingRules = filterRules update rules

filterRules :: Update -> [Rule] -> [Rule]
filterRules update = filter (\(r1,r2) -> r1 `elem` update && r2 `elem` update)

-- Parsers --

rulesParser :: String -> [Rule]
rulesParser []  = []
rulesParser ('\n':ss) = []
rulesParser s = (read num1, read num2) : rulesParser rest :: [Rule]
  where (num1,num2) = breakExclChar (=='|') nums
        (nums, rest) = breakExclChar (=='\n') s 

updateParser :: String -> [[Int]]
updateParser []  = []
updateParser (s:ss) 
  | s == '\n' = updateParser ss
updateParser s = parseUpdateLine line : updateParser rest
  where (line,rest) = break (== '\n') s

parseUpdateLine :: String -> [Int]
parseUpdateLine []  = []
parseUpdateLine (s:ss) 
  | s == ',' = parseUpdateLine ss
parseUpdateLine s = read num : parseUpdateLine rest :: [Int]
  where (num,rest) = break (== ',') s

getUpdates :: String -> String
getUpdates []  = []
getUpdates ('\n':'\n':updates) = updates
getUpdates (s:ss) = getUpdates ss

breakExclChar :: forall a. (a -> Bool) -> [a] -> ([a], [a])
breakExclChar f s = (fst, snds)
  where (fst, snd:snds) = break f s