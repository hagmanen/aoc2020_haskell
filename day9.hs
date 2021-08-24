import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import Data.List (tails)
import Data.Set (Set, fromList, member)

pairSums :: [Int] -> Set Int
pairSums l = fromList [x+y | (x:ys) <- tails l, y <- ys]

validInt :: Int -> [Int] -> Bool
validInt i preamble = member i $ pairSums preamble

findInvalid :: Int -> Int -> [Int] -> Int
findInvalid preamble ix numbers
  | not (validInt n (take preamble $ drop ix numbers)) = n
  | otherwise = findInvalid preamble (ix + 1) numbers
   where n = numbers !! (ix + preamble)

findRange :: Int -> (Int, Int) -> [Int] -> (Int, Int)
findRange target range numbers
  | target == 0 = range
  | target < 0 = findRange (target + (numbers !! fst range)) (fst range + 1, snd range) numbers
  | target > 0 = findRange (target - (numbers !! snd range)) (fst range, snd range + 1) numbers

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input9.txt" ReadMode
        contents <- hGetContents handle
        let numbers = [read l :: Int | l <- lines contents]
        let invalid = findInvalid 25 0 numbers
        print invalid
        let ixRange = findRange invalid (0, 0) numbers
        let range = take ((snd ixRange) - (fst ixRange)) $ drop (fst ixRange) numbers
        print ((maximum range) + (minimum range))
        hClose handle   

-- 133015568
-- 16107959
