import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List (sort)

diff :: [Int] -> [Int]
diff [] = []
diff [x] = []
diff (x:xs) = head xs - x: diff xs

count :: Int -> [Int] -> Int
count x = length . filter (x==)

factor :: Int -> Integer
factor x 
  | x < 2 = 1
  | x == 2 = 2
  | x == 3 = 4
  | x == 4 = 7
  | otherwise = 0

permutations :: Int -> [Int] -> Integer
permutations reps (1:ns) = permutations (reps + 1) ns
permutations reps (3:ns) = factor reps * permutations 0 ns
permutations reps [] = 1
permutations _ _ = 0

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input10.txt" ReadMode
        contents <- hGetContents handle
        let numbers = sort [read l :: Int | l <- lines contents]
        let diffs = diff (0:numbers) ++ [3]
        print (count 1 diffs * count 3 diffs)
        print (permutations 0 diffs)
        hClose handle   

-- 2046
-- 1157018619904
