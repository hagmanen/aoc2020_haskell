import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.Char (digitToInt)
import Data.List (foldl', sort)

char2bit :: Char -> Char
char2bit 'R' = '1'
char2bit 'B' = '1'
char2bit _ = '0'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parseSeat :: String -> Int
parseSeat s = toDec [char2bit c | c <- s]

findGap :: [Int] -> Int 
findGap [] = -1
findGap (s:ss) = if s + 1 /= head ss
  then s + 1
  else findGap ss

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input5.txt" ReadMode
        contents <- hGetContents handle
        let seats = map parseSeat (lines contents)
        print (maximum seats)
        print (findGap $ sort seats)
        hClose handle   
-- 835
-- 649
