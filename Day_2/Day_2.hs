import System.IO
import Debug.Trace (trace)
import Data.List (sort)
import Language.Haskell.TH (safe, pprint)

main = do
  contents <- readFile "input.txt"
  let lines = getArray contents (== '\n')
  let array = [getArray x (== ' ') |x <- lines]
  let arrayInt = map (map read) array :: [[Int]]
  let safeReportsArr = map safeReportDampener arrayInt
  let amountSafeReports = length (filter (==True) safeReportsArr)
  print amountSafeReports

safeReportDampener :: [Int] -> Bool
safeReportDampener [] = False
safeReportDampener report = or [safeReport (removeAt n report) | n <- [0..length report]]

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt n (x:xs) 
  | n == 0 = removeAt (-1) xs
  | otherwise = x: removeAt (n-1) xs

safeReport :: [Int] -> Bool
safeReport [] = False
safeReport (x:y:z:zs)
  | (safeAscending || safeDescending) && safeDistance x y = safeReport (y:z:zs)
  | otherwise = False
  where safeAscending = x > y && y > z
        safeDescending = x < y && y < z
safeReport [x,y] = safeDistance x y
safeReport x = True 

safeDistance :: Int -> Int -> Bool
safeDistance x y = abs (x-y) <= 3

getArray :: String -> (Char -> Bool) -> [String]
getArray [] _ = []
getArray (x:xs) f 
  | f x = getArray xs f
  | otherwise = broken : getArray rest f
  where (broken,rest) = break f (x:xs)


