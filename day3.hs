import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  

parseRow :: String -> [Bool]
parseRow s = cycle [c == '#' | c <- s]

score :: Bool -> Int
score True = 1
score False = 0

solve :: Int-> Int-> [[Bool]] -> Int 
solve _ _ [] = 0
solve s x (r:rs) = score (r !! x) + solve s (x + s) rs

skipEven :: [a] -> [a]
skipEven (x:y:xs) = x : skipEven xs;
skipEven _ = []

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input3.txt" ReadMode
        contents <- hGetContents handle
        let tree_map = map parseRow (lines contents)
        print (solve 3 0 tree_map)
        print (solve 1 0 tree_map * 
               solve 3 0 tree_map *
               solve 5 0 tree_map *
               solve 7 0 tree_map *
               solve 1 0 (skipEven tree_map))
        hClose handle   

-- 169
-- 7560370818
