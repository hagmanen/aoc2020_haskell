import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HashMap

parseKeyValue :: String -> (String, String)
parseKeyValue s = (head p, last p)
  where p = splitOn ":" s

parseRow :: String -> [(String, String)]
parseRow "" = []
parseRow s = [parseKeyValue kv | kv <- splitOn " " s]

groupPassports :: [[(String, String)]] -> [[(String, String)]]
groupPassports [] = []
groupPassports [x] = [x]
groupPassports (x:xs) = if null (head xs)
   then x : groupPassports (tail xs)
   else groupPassports ((x ++ head xs) : tail xs)

validPassport1 :: HashMap.HashMap String String -> Bool
validPassport1 passport = HashMap.member "byr" passport &&
                          HashMap.member "iyr" passport &&
                          HashMap.member "eyr" passport &&
                          HashMap.member "hgt" passport &&
                          HashMap.member "hcl" passport &&
                          HashMap.member "ecl" passport &&
                          HashMap.member "pid" passport

validHight :: (Int, String) -> Bool 
validHight (h, "cm") = h >= 150 && h <= 193
validHight (h, "in") = h >= 59 && h <= 76
validHight _ = False

validColor :: String -> Bool 
validColor hcl = head hcl == '#' && all (== True) [c `elem` (['0'..'9'] ++ ['a'..'f']) | c <- tail hcl]

validAttribute :: String -> Maybe String -> Bool
validAttribute _ Nothing = False
validAttribute "byr" (Just byr) = x >= 1920 && x <= 2002
  where x = read byr :: Integer
validAttribute "iyr" (Just iyr) = x >= 2010 && x <= 2020
  where x = read iyr :: Integer
validAttribute "eyr" (Just eyr) = x >= 2020 && x <= 2030
  where x = read eyr :: Integer
validAttribute "hgt" (Just hgt) = validHight (read (fst nu) :: Int, snd nu)
  where nu = (init (init hgt), reverse (take 2 (reverse hgt)))
validAttribute "hcl" (Just hcl) = validColor hcl
validAttribute "ecl" (Just ecl) = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validAttribute "pid" (Just pid) = length pid == 9 && all (== True) [c `elem` ['0'..'9'] | c <- pid]
validAttribute _ _ = True

validPassport2 :: HashMap.HashMap String String -> Bool
validPassport2 passport = validAttribute "byr" (HashMap.lookup "byr" passport) &&
                          validAttribute "iyr" (HashMap.lookup "iyr" passport) &&
                          validAttribute "eyr" (HashMap.lookup "eyr" passport) &&
                          validAttribute "hgt" (HashMap.lookup "hgt" passport) &&
                          validAttribute "hcl" (HashMap.lookup "hcl" passport) &&
                          validAttribute "ecl" (HashMap.lookup "ecl" passport) &&
                          validAttribute "pid" (HashMap.lookup "pid" passport)

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input4.txt" ReadMode
        contents <- hGetContents handle
        let passports = map HashMap.fromList (groupPassports (map parseRow (lines contents)))
        print (length [validPassport1 p | p <- passports, validPassport1 p])
        print (length [validPassport2 p | p <- passports, validPassport2 p])
        hClose handle   
-- 256
-- 198
