import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.List (foldr, intersect, union)
import qualified Data.HashMap.Strict as HashMap

join :: String -> [String] -> String
join sep = Data.List.foldr (\a b-> a ++ if b=="" then b else sep ++ b) ""

parseNrOfBags :: String -> (Int, String)
parseNrOfBags s = (read (head words) :: Int, join " " (Prelude.take 2 (tail words)))
  where words = splitOn " " s

parseContents :: String -> [(Int, String)]
parseContents "no other bags." = []
parseContents s = [parseNrOfBags b | b <- splitOn ", " s]

parseBag :: String -> (String, [(Int, String)])
parseBag s = (head r, parseContents (last r))
  where r = splitOn " bags contain " s

containsGold :: String -> HashMap.HashMap String [(Int,String)] -> Bool
containsGold "shiny gold" _ = True
containsGold bag bags = or ([containsGold (snd b) bags | b <- HashMap.lookupDefault [] bag bags])

countBagsWithGold :: HashMap.HashMap String [(Int,String)] -> Int
countBagsWithGold bags = sum [1 | b <- HashMap.keys bags, b /= "shiny gold" && containsGold b bags]

countBagsWithin :: [(Int,String)] -> HashMap.HashMap String [(Int,String)] -> Int
countBagsWithin [] _ = 0
countBagsWithin contents bags = sum [fst c * (1 + countBagsWithin (HashMap.lookupDefault [] (snd c) bags) bags) | c <- contents]

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input7.txt" ReadMode
        contents <- hGetContents handle
        let bags = HashMap.fromList [parseBag l | l <- lines contents]
        print (countBagsWithGold bags)
        print (countBagsWithin (HashMap.lookupDefault [] "shiny gold" bags) bags)
        hClose handle   
-- 155
-- 54803
