import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.List (foldr1, intersect, union)

uniqueAnswers :: String -> String
uniqueAnswers s = foldr1 union (lines s)

commonAnswers :: String -> String
commonAnswers s = foldr1 intersect (lines s)

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input6.txt" ReadMode
        contents <- hGetContents handle
        let groups = splitOn "\n\n" contents
        print (sum [length g | g <- map uniqueAnswers groups])
        print (sum [length g | g <- map commonAnswers groups])
        hClose handle   
-- 6457
-- 3260
