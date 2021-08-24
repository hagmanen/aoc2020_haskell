import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Data.List.Split (splitOn)
import Data.Set ( Set, insert, member, empty )
import System.Posix.Files.ByteString (accessTime)

parseInstruction :: String -> (String, Int)
parseInstruction s = (head ss, read $ last ss :: Int)
  where ss = splitOn " " [c | c <- s, c /= '+']

execInstruction :: (Int, Int) -> [(String, Int)] -> (Int, Int)
execInstruction (addr, acc) program 
  | fst inst == "nop" = (addr + 1, acc)
  | fst inst == "jmp" = (addr + snd inst, acc)
  | fst inst == "acc" = (addr + 1, acc + snd inst)
  | otherwise = (-1, 0)
  where inst = program !! addr

run :: (Int, Int) -> Set Int -> [(String, Int)] -> (Bool, Int)
run inst@(addr, acc) done program
  | member addr done = (False, acc)
  | addr == length program = (True, acc)
  | addr < 0 && addr > length program = (False, 0)
  | otherwise = run (execInstruction inst program) (insert addr done) program

mutateInst :: (String, Int) -> (String, Int)
mutateInst ("nop", i) = ("jmp", i)
mutateInst ("jmp", i) = ("nop", i)
mutateInst x = x

mutateProgram :: Int -> [(String, Int)] -> [(String, Int)]
mutateProgram patch program = fst p ++ mutateInst (head (snd p)) : tail (snd p)
  where p = splitAt patch program

runWithPatch :: Int -> [(String, Int)] -> (Bool, Int)
runWithPatch patch program
  | fst result = result
  | otherwise = runWithPatch (patch + 1) program
  where result = run (0, 0) empty (mutateProgram patch program)

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input8.txt" ReadMode
        contents <- hGetContents handle
        let program = [parseInstruction l | l <- lines contents]
        print (run (0, 0) empty program)
        print (runWithPatch 0 program)
        hClose handle   
-- 1749
-- 515
