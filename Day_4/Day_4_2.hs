import qualified Data.Map as Map

main = do
  contents <- readFile "input.txt"
  let vector = getVector (0,0) contents
  print $ findXMasCount vector vector

type CartVectChar = (Int,Int,Char)

findXMasCount :: [CartVectChar] -> [CartVectChar] -> Int
findXMasCount [] _ = 0
findXMasCount (cvc:cvcs) all
  | let (_,_,c) = cvc in c /= 'A' = findXMasCount cvcs all
  | otherwise = getValidXmas count + findXMasCount cvcs all
  where count = map getMatch starString
        starString = map (map (\(x,y,c) -> c)) star
        star = getStar shifted 
        shifted = newRefPoint cvc all

getValidXmas :: [Int] -> Int
getValidXmas [] = 0
getValidXmas (x:xs:xss) = if x + xs == 2 then 1 else 0 + getValidXmas xss

getVector :: (Int, Int) -> String -> [CartVectChar]
getVector (x,y) [] = []
getVector (x,y) (s:ss)
  | s == '\n' = getVector (0, y+1) ss 
  | otherwise = (x,y,s) : getVector (x+1,y) ss

getStar :: [CartVectChar] -> [[CartVectChar]]
getStar [] = []
getStar cvcs = map (\f -> filter f smallcvcs) filters
  where filters = [isDiagonal, isAntiDiagonal] 
        smallcvcs = filter fSmall cvcs
        fSmall (x,y,c)= inRange x && inRange y
        inRange n = n >= -1 && n <= 1
        isDiagonal (x,y,_) = x == y
        isAntiDiagonal (x,y,_) = x == -y

newRefPoint :: CartVectChar -> [CartVectChar] -> [CartVectChar]
newRefPoint (xoffset,yoffset,_) = map (\(x,y,c) -> (x -xoffset, y -yoffset, c))

getMatch :: String -> Int
getMatch s = match1 + match2
  where match1 = if s == "SAM" then 1 else 0
        match2 = if s == "MAS" then 1 else 0