import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Control.Monad ()

solveForHead :: Int -> [Int] -> Int
solveForHead target [] = 0
solveForHead target (x:xs) = head ([x * n | n <- xs, x  + n == target] ++ [0])

solve :: Int -> [Int] -> Int
solve _ [] = 0
solve target all@(x:xs) = if solveForHead target all /= 0 then
  solveForHead target all
    else
  solve target xs

solve2 :: Int -> [Int] -> Int
solve2 _ [] = 0
solve2 target (x:xs) = if solve (target - x) xs /= 0 then
  x * solve (target - x) xs
    else
  solve2 target xs

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input1.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        print (solve 2020 list)
        print (solve2 2020 list)
        hClose handle   

f :: [String] -> [Int]
f = map read

-- 806656
-- 230608320