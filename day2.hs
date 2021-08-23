import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Control.Monad (void)
import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, anyChar)
import Text.Parsec.Combinator (many1)
import Control.Applicative (many)
import Data.Char (isLetter, isDigit)
import Data.Either (rights)

data Policy = Policy
    { pIx1  :: Int
    , pIx2  :: Int
    , pChar :: Char
    , pPass :: String
    }
  deriving (Show, Eq)

policyParser :: Parser Policy
policyParser = do
    n <- many1 digit
    void $ char '-'
    m <- many1 digit
    void $ char ' '
    c <- anyChar
    void $ char ':'
    void $ char ' '
    rest <- many anyChar
    return (Policy (read n) (read m)  c  rest)

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

validPolicy1 :: Policy -> Bool
validPolicy1 (Policy pIx1 pIx2 pChar pPass) = n >= pIx1 && n <= pIx2
  where
    n = countLetters pPass pChar

solve1 :: [Policy] -> Int
solve1 ps = length [p | p <- ps, validPolicy1 p]

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

validPolicy2 :: Policy -> Bool
validPolicy2 (Policy pIx1 pIx2 pChar pPass) = (pPass !! (pIx1-1) == pChar) /= (pPass !! (pIx2-1) == pChar)

solve2 :: [Policy] -> Int
solve2 ps = length [p | p <- ps, validPolicy2 p]

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input2.txt" ReadMode
        contents <- hGetContents handle
        let policies = rights (map (parse policyParser "") (lines contents))
        print (solve1 policies)
        print (solve2 policies)
        hClose handle   

-- 483
-- 482