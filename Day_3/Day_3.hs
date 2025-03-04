import System.IO
import Language.Haskell.TH (safe, pprint)
import GHC.Read (Read(readPrec))

main = do
  contents <- readFile "input.txt"
  let values = getMulPattern contents True
  let splitval = map (\s ->  getOnlyInts s) values
  let splitIntVal = map (\(x,y) -> (read x, read y)) splitval :: [(Int,Int)]
  let mulval = map (\(x,y) -> x * y) splitIntVal
  print $ sum mulval

getDo :: String -> Bool
getDo s = match == doo
  where (match,rest) = getPattern("",s) doo
        doo = "do()"

getDont :: String -> Bool
getDont s = match /= dont
  where (match,rest) = getPattern("",s) dont
        dont = "don't()"

getMulPattern :: String -> Bool -> [String]
getMulPattern [] _ = []
getMulPattern (x:xs) b
  | not b = getMulPattern xs $ getDo $ x:xs 
  | match == "" =  getMulPattern xs $ getDont $ x:xs
  | otherwise = match : getMulPattern rest b
  where (match, rest) = getClosingBracket . getInt . getComma . getInt . getMul $ (x:xs)

getClosingBracket :: (String, String) -> (String, String)
getClosingBracket ("",r) = ("",r)
getClosingBracket s = getPattern s ")"

getComma :: (String,String) -> (String, String)
getComma ("",r) =  ("",r)
getComma s = getPattern s ","

getMul :: String -> (String, String)
getMul s = getPattern("",s) "mul("

getInt :: (String,String) -> (String,String)
getInt ("",r) = ("",r)
getInt (m,[]) = (m,"") 
getInt (m,r:rs) 
  | or [True | n <- ['0'..'9'], n == r] = getInt (m ++ [r], rs) 
  | or [True | n <- ['0'..'9'], n == last m] = (m, r:rs)
  | otherwise = ("", r:rs)

getIntMiddle :: String -> String
getIntMiddle s = fst $ getIntMiddle' ("",s)
  where getIntMiddle' :: (String,String) -> (String,String)
        getIntMiddle' (m,[]) = (m,"") 
        getIntMiddle' (m,r:rs) 
          | or [True | n <- ['0'..'9'], n == r] = getInt (m ++ [r], rs) 
          | otherwise = getIntMiddle' (m,rs) 

getPattern :: (String,String) -> String -> (String,String)
getPattern (m,[]) _ = (m,[])
getPattern (m,r:rs) (x:xs) 
  | r == x =  getPattern (m ++ [x],rs) xs
  | otherwise = ("", r:rs)
getPattern (m,r) [] = (m,r)

getOnlyInts :: String -> (String,String)
getOnlyInts s = (getIntMiddle before, getIntMiddle after)
  where (before,after) = break (== ',') s